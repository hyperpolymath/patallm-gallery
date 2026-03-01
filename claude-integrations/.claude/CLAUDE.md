# CLAUDE.md - Claude Integrations

## Project Overview

Monorepo for all Claude AI browser and service integrations, plus AI workflow tools.

## Components

| Directory | Description | Stack |
|-----------|-------------|-------|
| firefox-lsp/ | LSP for Firefox via Marionette protocol | Elixir |
| firefox-mcp/ | MCP server for Firefox browser control | Deno/JS |
| gecko-browser-extension/ | Extension for Gecko browsers | JS/WebExtensions |
| gitlab-bridge/ | Claude ↔ GitLab integration | ReScript/Deno |
| mozilla-extension/ | Claude extension for Mozilla products | JS/WebExtensions |
| **model-router/** | **Auto-select Opus/Sonnet/Haiku by task** | **Rust (planned)** |

## Build

```bash
just --list                    # See all build recipes
just build-firefox-lsp         # Build Firefox LSP
just build-firefox-mcp         # Build Firefox MCP
just build-gecko-extension     # Build Gecko extension
just build-gitlab-bridge       # Build GitLab bridge
just build-mozilla-extension   # Build Mozilla extension
```

## Model Router (NEW - Sonnet Task)

### Purpose

Auto-select the right Claude model (Opus/Sonnet/Haiku) based on task complexity, saving money on simple tasks and ensuring quality on complex ones.

### Architecture

```
User prompt + repo context
        ↓
Haiku classifier (< 1 second, < $0.001)
        ↓
Classification: trivial | standard | complex
        ↓
Route to:
  ├── Haiku:  single-file edits, template creation, simple queries
  ├── Sonnet: multi-file implementation, feature work, testing
  └── Opus:   architecture decisions, debugging, cross-repo design
```

### Classification Signals

| Signal | Haiku | Sonnet | Opus |
|--------|-------|--------|------|
| Files affected | 1 | 2-5 | 5+ |
| Task type | rename, typo fix | add feature, implement | debug, design, architect |
| Prompt keywords | "fix typo", "rename", "update" | "add", "create", "implement" | "why", "how", "design", "debug" |
| Repo complexity (from CLAUDE.md) | Low | Medium | High |
| Error recovery needed | No | Maybe | Likely |
| Cross-repo awareness | No | No | Yes |

### Implementation Plan (for Sonnet)

1. **Create `model-router/` directory** in this repo
2. **Rust CLI tool** that:
   - Reads the prompt from stdin or argument
   - Reads `.claude/CLAUDE.md` from current directory for repo context
   - Calls Haiku API to classify complexity (one fast API call)
   - Outputs recommended model to stdout: `haiku`, `sonnet`, or `opus`
3. **Claude Code hook integration**:
   - Pre-prompt hook reads model recommendation
   - Switches model via `/model` command or API parameter
4. **Escalation logic**:
   - If chosen model fails or stalls (detected by output quality heuristics)
   - Automatically escalate: Haiku → Sonnet → Opus
5. **Cost tracking**:
   - Log which model was used, tokens consumed, task outcome
   - Weekly summary of cost savings vs always-using-Opus

### Cargo.toml Sketch

```toml
[package]
name = "model-router"
version = "0.1.0"
authors = ["Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"]
license = "PMPL-1.0-or-later"

[dependencies]
clap = { version = "4", features = ["derive"] }
reqwest = { version = "0.12", features = ["json"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
tokio = { version = "1", features = ["full"] }
```

### Usage (Target)

```bash
# Classify a task
echo "fix the typo in README.md" | model-router classify
# Output: haiku

echo "implement the sweep subcommand for panic-attack" | model-router classify
# Output: sonnet

echo "why is my system crashing and how should we architect the fix" | model-router classify
# Output: opus

# With repo context
model-router classify --repo /path/to/repo "add authentication"
# Reads .claude/CLAUDE.md, factors in repo complexity
```

## Code Style

- SPDX headers: `PMPL-1.0-or-later`
- Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
- Browser extensions use `MPL-2.0` fallback (store requirements)
