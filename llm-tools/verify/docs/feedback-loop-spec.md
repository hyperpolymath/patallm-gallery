# Claude Offline Learning & Verification Feedback System

## Specification v0.1

### 1. Problem Statement

Claude generates code based on pattern matching against training data, not formal verification. This means:

- Claude can and does make mistakes (off-by-one, type mismatches, logic errors)
- The same mistakes may recur across sessions (no persistent learning)
- Users have no systematic way to capture and communicate what went wrong
- Corrections don't persist or improve future Claude behavior

Users have no way to:
1. Capture what Claude got wrong with proper provenance
2. Feed corrections back to Claude within a session
3. Feed corrections to Anthropic for training improvement
4. Trigger exhaustive verification automatically
5. Build project-specific "known issues" databases

### 2. Proposed Solution

A feedback capture and injection system that:

1. **Captures** when Claude makes mistakes (with source, category, correction)
2. **Stores** feedback in a local database with full provenance
3. **Injects** known issues into future Claude sessions
4. **Optionally shares** anonymized feedback with Anthropic

### 3. Components

#### 3.1 Local Verification Daemon (`claude-verify`)

A background service that:
- Watches for file changes in project directories
- Runs language-appropriate verification suite
- Generates structured reports (JSON/SARIF)
- Caches results to avoid redundant runs
- Integrates with ECHIDNA for formal verification

**Interfaces:**
- CLI: `claude-verify run --project /path/to/project`
- Socket: `/run/claude-verify.sock` for editor integration
- HTTP: `localhost:9847/api/verify` for browser plugin

#### 3.2 Claude Context Injection File (`.claude-context.toml`)

Per-project file that Claude reads at conversation start:

```toml
[project]
name = "my-critical-system"
languages = ["rust", "ada"]
safety_level = "high"

[known_issues]
items = [
    "Off-by-one in pagination - always use Range not manual arithmetic",
    "This project uses no_std - don't assume alloc is available",
    "DateTime handling must use chrono with explicit timezone",
]

[verification]
required = ["compile", "clippy", "test", "miri"]
optional = ["audit", "deny", "doc"]

[formal_specs]
tla_plus = ["specs/consensus.tla"]
alloy = ["specs/access_control.als"]
```

#### 3.3 Feedback Database Schema

```sql
CREATE TABLE feedback (
    id TEXT PRIMARY KEY,           -- UUID
    timestamp TEXT NOT NULL,       -- ISO 8601
    project TEXT,                  -- Project name
    session_id TEXT,               -- Claude session ID
    category TEXT NOT NULL,        -- syntax_error, type_error, logic_error, etc.
    subcategory TEXT,              -- More specific classification
    source_file TEXT,              -- File where issue occurred
    source_line_start INTEGER,     -- Start line
    source_line_end INTEGER,       -- End line
    claude_output TEXT NOT NULL,   -- What Claude generated
    actual_issue TEXT NOT NULL,    -- What was wrong
    correction TEXT,               -- What the fix was
    detected_by TEXT NOT NULL,     -- compiler, test, prover, user
    verified INTEGER DEFAULT 0,    -- Was the correction verified?
    shared_with_anthropic INTEGER DEFAULT 0
);
```

#### 3.4 Feedback Capture Format (JSON)

```json
{
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "timestamp": "2025-11-29T14:32:00Z",
    "project": "my-critical-system",
    "session_id": "claude-session-abc123",
    "feedback_type": "correction",
    "claude_output": {
        "file": "src/lib.rs",
        "line_range": [45, 52],
        "content": "for i in 0..=n { ... }"
    },
    "issue": {
        "category": "logic_error",
        "subcategory": "off_by_one",
        "description": "Loop should be 0..n not 0..=n",
        "detected_by": "test_suite"
    },
    "correction": {
        "content": "for i in 0..n { ... }",
        "verified": true,
        "verification_output": "All tests pass"
    }
}
```

### 4. Data Flow

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   Claude Chat   │────▶│  claude-verify   │────▶│  Local Report   │
│   (generates)   │     │  (validates)     │     │  (stores)       │
└─────────────────┘     └──────────────────┘     └─────────────────┘
        │                        │                        │
        │                        ▼                        │
        │               ┌──────────────────┐              │
        │               │  Feedback Store  │◀─────────────┘
        │               │  (SQLite)        │
        │               └──────────────────┘
        │                        │
        ▼                        ▼
┌─────────────────┐     ┌──────────────────┐
│  Next Claude    │◀────│ Context Injector │
│  Session        │     │ (reads known     │
│                 │     │  issues)         │
└─────────────────┘     └──────────────────┘
        │
        ▼ (optional, with consent)
┌─────────────────┐
│   Anthropic     │
│   Feedback API  │
└─────────────────┘
```

### 5. Integration Points

#### 5.1 Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Run claude-verify on staged files
claude-verify check --staged --fail-on-error

if [ $? -ne 0 ]; then
    echo "Verification failed. Commit aborted."
    exit 1
fi
```

#### 5.2 Browser Extension

For Claude.ai web interface:
- Detects when Claude generates code
- Injects `.claude-context.toml` into system prompt
- Adds "Verify" button to check generated code
- Captures feedback when user corrects Claude

#### 5.3 Claude Code Integration

Slash command in Claude Code:
```
/verify [files...]     - Run full verification
/feedback record       - Record a correction
/feedback list         - Show known issues
```

#### 5.4 CI/CD Integration

GitHub Action:
```yaml
name: Claude Verify
on: [push, pull_request]

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: hyperpolymath/claude-verify-action@v1
        with:
          echidna-version: 'latest'
          fail-on: ['counterexample', 'error']
          upload-feedback: true
```

### 6. Privacy & Consent

All data is local by default. For sharing with Anthropic:

1. **Explicit opt-in** required per-project in `.claude-context.toml`
2. **Anonymization** before sharing:
   - Remove project name
   - Remove file paths
   - Remove session IDs
   - Strip comments that might contain PII
3. **Project-level "never share" flag** respected
4. **User can review** before any data leaves the machine

### 7. Feedback Categories

| Category | Description | Detection Method |
|----------|-------------|------------------|
| `syntax_error` | Code doesn't parse | Compiler/parser |
| `type_error` | Type mismatch | Type checker |
| `logic_error` | Incorrect behavior | Tests, provers |
| `security_flaw` | Vulnerability | Static analysis |
| `performance_issue` | Inefficient code | Profiler |
| `semantic_mismatch` | Doesn't match spec | Formal verification |
| `style_violation` | Doesn't match conventions | Linter |
| `api_misuse` | Wrong API usage | Type checker, docs |
| `missing_error_handling` | Unhandled cases | Static analysis |
| `concurrency_issue` | Race/deadlock | Model checker |

### 8. Known Issue Format

For injection into Claude sessions:

```markdown
## Known Issues for my-project

### Patterns to Avoid

- **logic_error:off_by_one** (5 occurrences)
  Loop bounds often wrong. Use exclusive upper bounds.
  Guidance: Always use 0..n not 0..=n unless explicitly needed

- **type_error:option_unwrap** (3 occurrences)
  Unwrapping Options without checking.
  Guidance: Use if let or match, never unwrap()

### Recent Corrections

- [logic_error] Pagination used >= instead of > for last page check
- [type_error] Used String where &str was expected
```

### 9. ECHIDNA Integration

Claude-verify delegates formal verification to ECHIDNA:

```
┌──────────────────────────────────────────────────────────────────┐
│                          ECHIDNA                                  │
│                                                                   │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐             │
│  │   Z3    │  │  CVC5   │  │  Lean   │  │   Coq   │   ...       │
│  └─────────┘  └─────────┘  └─────────┘  └─────────┘             │
│       │            │            │            │                   │
│       └────────────┴────────────┴────────────┘                   │
│                           │                                       │
│                    ┌──────┴──────┐                                │
│                    │  Portfolio  │                                │
│                    │  Manager    │                                │
│                    └──────┬──────┘                                │
│                           │                                       │
│               ┌───────────┴───────────┐                           │
│               │    Neurosymbolic      │                           │
│               │    Assessment         │                           │
│               │ (DeepProbLog+OpenCyc) │                           │
│               └───────────┬───────────┘                           │
│                           │                                       │
│                    Unified Result                                 │
└──────────────────────────────────────────────────────────────────┘
```

### 10. Implementation Status

| Component | Status | Notes |
|-----------|--------|-------|
| Haskell claude-verify library | In progress | Core types, ECHIDNA client |
| Rust parser | Partial | Basic AST extraction |
| Haskell parser | Partial | Basic AST extraction |
| Ada/SPARK parser | Partial | Basic AST extraction |
| Feedback database | Complete | SQLite schema, CRUD ops |
| CLI | Complete | verify, check, feedback, init |
| Pre-commit hook | Template | Needs testing |
| Browser extension | Not started | Planned |
| Claude Code integration | Not started | Planned |
| GitHub Action | Not started | Planned |

### 11. Open Questions

1. **Echomesh Integration**: Should feedback persist across the Echomesh context layer?

2. **Feedback Aggregation**: How do we aggregate similar issues? Fuzzy matching?

3. **Confidence Decay**: Should old feedback items lose weight over time?

4. **Cross-Project Learning**: Can we learn from issues in other projects (with consent)?

5. **Anthropic API**: What format does Anthropic want for training feedback?

### 12. Security Considerations

1. **Local Storage**: Feedback database contains code snippets - encrypt at rest?

2. **Network**: If sharing with Anthropic, use TLS, verify certificates

3. **Secrets**: Never capture feedback containing API keys, passwords

4. **Code Injection**: Sanitize feedback before injecting into prompts

---

*This specification is part of the claude-verify project.*
*For ECHIDNA integration details, see https://github.com/Hyperpolymath/echidna*
