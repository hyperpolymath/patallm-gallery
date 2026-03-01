# Claude-Verify Architecture

## The Honest Truth About LLM Code Validation

### What Claude Actually Does (Pattern Matching)

When Claude "validates" code, here's what's **actually happening**:

| What It Looks Like | What's Actually Happening |
|-------------------|---------------------------|
| "This code is correct" | High statistical confidence from training patterns |
| "I notice this bug" | Pattern recognition against seen error patterns |
| "The types match" | Internalised grammar patterns, not actual type inference |
| "This compiles" | Prediction based on syntax familiarity |
| "No security issues" | Known vulnerability pattern matching |

**There is no:**
- Pratt parser running
- SMT solver executing
- Model checker exploring state space
- Theorem prover constructing proofs
- Type inference algorithm computing
- Grammar checked against formal specification

### What Claude Could Do (With Tools)

In an environment with computer access, Claude can **orchestrate** real tools:

```
Claude Output → Real Parser → Real Type Checker → Real Prover → Actual Result
```

But Claude itself remains a prediction engine. The verification is done by the tools, not by Claude.

### The Gap This Project Fills

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         CURRENT STATE                                    │
│                                                                          │
│   User: "Is this code correct?"                                          │
│   Claude: "Yes" (pattern matching, 85-95% accurate on common patterns)   │
│   Reality: Unknown until actually verified                               │
│                                                                          │
├─────────────────────────────────────────────────────────────────────────┤
│                         TARGET STATE                                     │
│                                                                          │
│   User: "Is this code correct?"                                          │
│   Claude: *invokes claude-verify*                                        │
│   claude-verify: *calls ECHIDNA*                                         │
│   ECHIDNA: *runs Z3, CVC5, Lean, Coq, etc.*                             │
│   Result: "Proved correct" | "Counterexample found" | "Unknown"         │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## Architecture Overview

### Component Stack

```
┌─────────────────────────────────────────────────────────────────────────┐
│                                                                          │
│   Layer 4: User Interface                                                │
│   ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                  │
│   │ Claude Chat  │  │ Browser Ext  │  │  Git Hooks   │                  │
│   └──────────────┘  └──────────────┘  └──────────────┘                  │
│          │                  │                  │                         │
│          └──────────────────┼──────────────────┘                         │
│                             ▼                                            │
│   Layer 3: Orchestration (claude-verify)                                 │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  claude-verify (Haskell)                                         │   │
│   │  - Language detection & parsing                                  │   │
│   │  - Property extraction                                           │   │
│   │  - Verification condition generation                             │   │
│   │  - Feedback capture & injection                                  │   │
│   │  - Report generation                                             │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                             │                                            │
│                             ▼                                            │
│   Layer 2: Verification Backend (ECHIDNA)                                │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  ECHIDNA (Rust + Julia)                                          │   │
│   │  - 12 theorem provers (Z3, CVC5, Lean, Coq, Agda, Mizar...)     │   │
│   │  - DeepProbLog integration (neurosymbolic)                       │   │
│   │  - OpenCyc knowledge base                                        │   │
│   │  - Multi-solver portfolio strategy                               │   │
│   │  - Proof certificate generation                                  │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                             │                                            │
│                             ▼                                            │
│   Layer 1: Individual Provers                                            │
│   ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐          │
│   │  Z3   │ │ CVC5  │ │ Lean  │ │  Coq  │ │ Agda  │ │Vampire│ ...      │
│   └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘          │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Data Flow

```
                                    ┌──────────────────────┐
                                    │  .claude-context.toml │
                                    │  (project config)     │
                                    └──────────┬───────────┘
                                               │
┌────────────────┐                             │
│ Claude Output  │                             ▼
│ (source code)  │────────────────────▶ ┌─────────────────┐
└────────────────┘                      │  claude-verify   │
                                        │                  │
┌────────────────┐                      │  1. Parse code   │
│ Formal Specs   │────────────────────▶ │  2. Extract VCs  │
│ (TLA+, Alloy)  │                      │  3. Call ECHIDNA │
└────────────────┘                      │  4. Capture      │
                                        │     feedback     │
┌────────────────┐                      └────────┬────────┘
│ Known Issues   │◀───────────────────────────────┤
│ (feedback DB)  │                                │
└────────────────┘                                ▼
                                        ┌─────────────────┐
                                        │    ECHIDNA      │
                                        │                 │
                                        │  Multi-prover   │
                                        │  verification   │
                                        └────────┬────────┘
                                                 │
                              ┌──────────────────┼──────────────────┐
                              ▼                  ▼                  ▼
                    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
                    │   Proved    │    │ Counterex   │    │   Unknown   │
                    │ (+ witness) │    │ (+ model)   │    │ (+ timeout) │
                    └─────────────┘    └─────────────┘    └─────────────┘
                              │                  │                  │
                              └──────────────────┼──────────────────┘
                                                 ▼
                                        ┌─────────────────┐
                                        │  Verification   │
                                        │    Report       │
                                        │  (SARIF/JSON)   │
                                        └─────────────────┘
```

## Why Haskell for claude-verify?

| Requirement | Why Haskell |
|-------------|-------------|
| **Real parsing** | Megaparsec, Parsec — industrial-strength parser combinators |
| **Real SMT** | SBV, what4 — direct bindings to Z3, CVC5, Yices2, MathSAT |
| **Type safety** | Strong static types catch integration bugs at compile time |
| **Property extraction** | Algebraic data types model VCs naturally |
| **DSL embedding** | Excellent for embedding specification languages |
| **Functional purity** | Verification code should be referentially transparent |

### Alternative Considered: OCaml

OCaml powers Coq, Why3, Frama-C — the formal verification ecosystem. We may port to OCaml later for deeper Why3 integration. But Haskell's SBV provides excellent SMT bindings for the initial implementation.

## ECHIDNA Integration

### Protocol

claude-verify communicates with ECHIDNA via:

1. **Command-line**: `echidna prove --input vc.smt2 --provers z3,cvc5,lean`
2. **gRPC** (planned): Streaming verification results
3. **JSON-RPC** (fallback): HTTP API for web integration

### Verification Task Format

```json
{
  "task_id": "uuid",
  "source_language": "rust",
  "verification_conditions": [
    {
      "vc_id": "bounds_check_line_42",
      "formula": "(assert (>= idx 0)) (assert (< idx len))",
      "format": "smtlib2",
      "source_location": {"file": "src/lib.rs", "line": 42}
    }
  ],
  "prover_preferences": {
    "prefer": ["z3", "cvc5"],
    "timeout_ms": 30000,
    "require_proof_certificate": true
  }
}
```

### Result Format

```json
{
  "task_id": "uuid",
  "status": "proved" | "counterexample" | "unknown" | "error",
  "results": [
    {
      "vc_id": "bounds_check_line_42",
      "prover": "z3",
      "status": "proved",
      "time_ms": 142,
      "proof_certificate": "...",
      "confidence": 1.0
    }
  ],
  "aggregate_confidence": 0.98,
  "neurosymbolic_assessment": {
    "deepprob_log_confidence": 0.94,
    "symbolic_coverage": 0.87
  }
}
```

## Feedback Loop

### Purpose

Claude makes mistakes. Those mistakes should:
1. Be captured with provenance
2. Be fed back into the current session (immediate)
3. Be stored for future sessions (persistence)
4. Optionally be shared with Anthropic (training improvement)

### Feedback Categories

| Category | Example | Detection Method |
|----------|---------|------------------|
| `syntax_error` | Missing semicolon | Compiler/parser |
| `type_error` | Wrong type passed | Type checker |
| `logic_error` | Off-by-one | Tests, provers |
| `security_flaw` | Injection vulnerability | Static analysis |
| `performance_issue` | O(n²) when O(n) possible | Profiler, analysis |
| `semantic_mismatch` | Code doesn't match spec | Formal verification |
| `style_violation` | Doesn't match project conventions | Linter |

### Storage

Local SQLite database with schema:

```sql
CREATE TABLE feedback (
    id TEXT PRIMARY KEY,
    timestamp TEXT NOT NULL,
    project TEXT,
    session_id TEXT,
    category TEXT NOT NULL,
    subcategory TEXT,
    source_file TEXT,
    source_lines TEXT,  -- JSON array
    claude_output TEXT,
    actual_issue TEXT,
    correction TEXT,
    detected_by TEXT,
    verified BOOLEAN,
    shared_with_anthropic BOOLEAN DEFAULT FALSE
);

CREATE INDEX idx_project ON feedback(project);
CREATE INDEX idx_category ON feedback(category);
```

## Integration Points

### 1. Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit
claude-verify check --staged --fail-on-error
```

### 2. Browser Extension

Injects `.claude-context.toml` contents into Claude.ai conversations:
- Known project issues
- Required verification checks
- Recent feedback items

### 3. Claude Code Desktop

Custom slash command:
```
/verify [files...]
```

Runs full verification suite and reports results in conversation.

### 4. CI/CD Integration

GitHub Action:
```yaml
- uses: hyperpolymath/claude-verify-action@v1
  with:
    echidna-version: 'latest'
    fail-on: ['counterexample', 'error']
    upload-feedback: true
```

## What This Doesn't Do

**This system cannot:**
- Make Claude actually understand code (still pattern matching)
- Guarantee absence of all bugs (verification is incomplete)
- Replace human review (humans understand requirements)
- Verify arbitrary properties (needs specifications)

**This system does:**
- Provide actual verification instead of guessing
- Capture and persist correction feedback
- Integrate real provers (Z3, CVC5, Lean, etc.)
- Give confidence levels with evidence
- Enable learning from mistakes (via feedback loop)

## Comparison with Other Approaches

| Approach | Pros | Cons |
|----------|------|------|
| **Claude alone** | Fast, convenient | No actual verification |
| **Run tests** | Catches many bugs | Tests may be incomplete |
| **Type checkers** | Sound (for what they check) | Limited to type safety |
| **Linters** | Catch common patterns | Heuristic, not proof |
| **Formal verification** | Mathematical proof | Requires specifications |
| **Claude + claude-verify + ECHIDNA** | Best of all worlds | Setup complexity |

## Roadmap

### Phase 1: Core (This Implementation)
- [x] Architecture document
- [ ] Haskell project structure
- [ ] ECHIDNA client module
- [ ] Basic VC extraction
- [ ] Feedback capture

### Phase 2: Language Support
- [ ] Rust parser and VC generator
- [ ] Haskell parser and VC generator
- [ ] Ada/SPARK parser
- [ ] Elixir parser

### Phase 3: Integration
- [ ] Pre-commit hook
- [ ] Browser extension
- [ ] Claude Code plugin
- [ ] GitHub Action

### Phase 4: Advanced
- [ ] Echomesh context integration
- [ ] Anthropic feedback API
- [ ] Community issue database
- [ ] Neurosymbolic confidence calibration

---

*This document is part of the claude-verify project. It exists because LLMs shouldn't pretend to verify things they can't verify.*
