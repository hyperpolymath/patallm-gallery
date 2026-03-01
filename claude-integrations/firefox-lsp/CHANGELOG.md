# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial project structure
- Firefox adapter using Marionette protocol
- LSP server with GenLSP framework
- 7 LSP commands: navigate, click, typeText, screenshot, executeJs, getContent, detectBrowsers
- Adapter behaviour contract
- VS Code extension scaffold
- Comprehensive documentation (README.adoc)
- Checkpoint files (STATE.scm, META.scm, ECOSYSTEM.scm)
- justfile for common tasks

### Technical Details
- Marionette protocol implementation with TCP socket communication
- GenServer-based adapter with persistent state
- Base64 screenshot encoding for JSON transport
- Supervisor tree for fault isolation

## [0.1.0] - 2026-02-05

### Added
- Initial project creation from poly-ssg-lsp template

[Unreleased]: https://github.com/hyperpolymath/claude-firefox-lsp/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/hyperpolymath/claude-firefox-lsp/releases/tag/v0.1.0
