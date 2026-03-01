# Claude Firefox LSP - Project Summary

**Created:** 2026-02-05
**Status:** Initial Development (15% complete to MVP)
**Location:** ~/Documents/hyperpolymath-repos/claude-firefox-lsp

## Overview

Claude Firefox LSP is a Language Server Protocol implementation for Firefox browser automation. It enables editor-integrated web testing, scraping, and automation using Firefox's native Marionette protocol.

## Architecture

### Core Components

1. **Adapter System** (lib/adapters/)
   - `behaviour.ex` - Adapter contract defining 8 callbacks
   - `firefox.ex` - Marionette protocol implementation (365 LOC)
   - `supervisor.ex` - GenServer supervision tree

2. **LSP Server** (lib/lsp/)
   - `server.ex` - GenLSP-based server with 7 commands (138 LOC)
   - Command execution and response formatting

3. **VS Code Extension** (vscode-extension/)
   - TypeScript client with command palette integration
   - Base64 screenshot handling
   - Configuration for server path and trace levels

## Key Features

### Implemented ✓

- **Navigate** - Load URLs with timeout and wait conditions
- **Click** - CSS selector/XPath element clicking
- **Type Text** - Form field text entry with clear option
- **Screenshot** - Full page or element capture (base64 encoded)
- **Execute JavaScript** - Arbitrary JS in page context
- **Get Content** - Extract HTML, text, or DOM representation
- **Detect Browsers** - Auto-detect available browsers

### Technical Highlights

- **Marionette Protocol**: Native Firefox automation via TCP (port 2828)
- **GenServer State**: Persistent session management with fault isolation
- **GenLSP Framework**: LSP protocol handling with STDIO transport
- **Supervisor Tree**: :one_for_one restart strategy for adapters
- **Base64 Screenshots**: JSON-compatible binary transport

## File Structure

```
claude-firefox-lsp/
├── lib/
│   ├── adapters/
│   │   ├── behaviour.ex         # Adapter contract
│   │   ├── firefox.ex           # Marionette implementation
│   │   └── supervisor.ex        # Adapter supervision
│   ├── claude_firefox_lsp/
│   │   └── application.ex       # OTP application
│   ├── lsp/
│   │   └── server.ex            # GenLSP server
│   └── claude_firefox_lsp.ex    # Main module
├── test/
│   ├── claude_firefox/
│   │   └── lsp_test.exs         # Basic tests
│   └── test_helper.exs
├── vscode-extension/
│   ├── src/
│   │   └── extension.ts         # VS Code client
│   ├── package.json             # Extension manifest
│   ├── tsconfig.json            # TypeScript config
│   └── README.md
├── CHANGELOG.md                 # Version history
├── ECOSYSTEM.scm                # Ecosystem positioning
├── justfile                     # Task automation
├── LICENSE                      # PMPL-1.0-or-later
├── META.scm                     # ADRs and design rationale
├── mix.exs                      # Elixir project config
├── README.adoc                  # User documentation
└── STATE.scm                    # Project state checkpoint
```

## Code Metrics

- **Total Lines**: 748 LOC (Elixir)
- **Modules**: 6 (5 lib + 1 test)
- **Adapters**: 1 (Firefox)
- **LSP Commands**: 7
- **Test Coverage**: Minimal (basic smoke tests only)

## Technology Stack

- **Language**: Elixir 1.17+
- **Framework**: GenLSP 0.10+
- **Protocol**: Marionette (Firefox native)
- **Transport**: TCP socket (port 2828)
- **Testing**: ExUnit, Mox
- **Quality**: Credo, Dialyzer, ExCoveralls
- **VS Code**: TypeScript, vscode-languageclient

## Dependencies

### Elixir (mix.exs)

```elixir
{:gen_lsp, "~> 0.10"}         # LSP framework
{:jason, "~> 1.4"}            # JSON parsing
{:req, "~> 0.5"}              # HTTP client (future use)
{:credo, "~> 1.7"}            # Linter
{:dialyxir, "~> 1.4"}         # Type checker
{:ex_doc, "~> 0.34"}          # Documentation
{:excoveralls, "~> 0.18"}     # Coverage
{:mox, "~> 1.1"}              # Mocking
```

### TypeScript (package.json)

```json
"vscode-languageclient": "^9.0.0"
"typescript": "^5.0.0"
```

## ADRs (Architecture Decision Records)

### ADR-001: Use Marionette Protocol

**Decision**: Use Marionette as primary Firefox automation interface

**Rationale**:
- Native Firefox protocol (actively maintained by Mozilla)
- Full browser automation capabilities
- Lower latency than HTTP-based protocols

**Trade-offs**:
- Requires manual Firefox startup with `-marionette` flag
- Protocol differs from Chrome (harder to unify multi-browser support)

### ADR-002: Use GenLSP Framework

**Decision**: Use GenLSP library for LSP server implementation

**Rationale**:
- Proven framework (used by Elixir community)
- Handles protocol boilerplate (JSON-RPC, STDIO)
- Active maintenance and documentation

### ADR-003: Base64 Screenshot Encoding

**Decision**: Base64 encode screenshots for JSON transport

**Rationale**:
- LSP is JSON-RPC based (text protocol)
- Maintains compatibility with standard LSP clients
- Avoids filesystem coupling

**Trade-offs**:
- 33% payload size increase
- Higher memory usage for large screenshots

## Usage

### Start Firefox with Marionette

```bash
firefox -marionette -headless
```

### Run LSP Server

```bash
just setup   # Install deps
just start   # Start server
```

### VS Code Commands

- `Firefox: Navigate to URL`
- `Firefox: Click Element`
- `Firefox: Type Text`
- `Firefox: Take Screenshot`
- `Firefox: Execute JavaScript`
- `Firefox: Get Page Content`
- `Firefox: Detect Available Browsers`

## Next Steps

### Phase 2: Polish (Pending)

- [ ] Implement VS Code extension client
- [ ] Add comprehensive tests (integration, e2e)
- [ ] Error handling and retry logic
- [ ] Connection pooling for multiple sessions
- [ ] Document Marionette protocol details

### Phase 3: Ecosystem Integration (Pending)

- [ ] Add Chrome adapter (CDP protocol)
- [ ] Add WebKit adapter (Safari)
- [ ] Hypatia CI/CD integration
- [ ] GitHub workflows (hypatia-scan, codeql, mirror, etc.)
- [ ] Publish to Hex.pm

## Related Projects

- **poly-ssg-lsp**: Template source (adapter pattern, GenLSP)
- **poly-queue-lsp**: Architectural sibling (LSP for message queues)
- **poly-observability-lsp**: Architectural sibling (LSP for observability)
- **Hypatia**: Neurosymbolic CI/CD intelligence
- **gitbot-fleet**: Automated repo maintenance

## License

SPDX-License-Identifier: PMPL-1.0-or-later
Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

Licensed under the Palimpsest License, version 1.0 or later.
