# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

#### New Parsers
- **Claude parser** - Full implementation supporting Anthropic export format
- **Gemini parser** - Supports Gem Chat Exporter and ai-chat-exporter formats
- **GitHub Copilot parser** - Supports VS Code "Chat: Export Chat..." format

#### Schema & Backup
- **Schema versioning** - Automatic migrations with version history
- **`schema` command** - Display current schema version and migration history
- **Backup integrity** - SHA-256 checksums for all backups
- **Atomic backups** - Temp file + rename pattern prevents corruption
- **Restore validation** - Pre-restore checksum and SQLite header verification
- **WAL mode** - Write-ahead logging for improved concurrency

#### Compliance
- **.well-known/security.txt** - RFC 9116 compliant security disclosure
- **.well-known/ai.txt** - AI training policies and ethical guidelines
- **.well-known/humans.txt** - Human attribution and credits
- **.config/well-known.scm** - Scheme configuration for .well-known resources
- **mustfile.ncl** - Nickel configuration for project requirements

#### E2E Testing
- **21 comprehensive E2E tests** covering all providers and features
- **Test fixtures** for ChatGPT, Claude, Gemini, and Copilot formats

### Changed
- Updated CLI from 13 to 14 commands (added `schema`)
- Improved `validate` command with SQLite integrity + data consistency checks
- Enhanced `version` command to show schema, backup, and export format versions
- Export now includes format metadata (version, schema, timestamp)

### Planned for v0.2.0
- SQLite database encryption (SQLCipher)
- Export file encryption (age/GPG)
- Enhanced TUI with message viewing
- 80%+ test coverage

## [0.1.0] - 2025-11-28

### Added

#### Core Features
- **Multi-platform support** - Import conversations from ChatGPT, Claude, Gemini, Copilot
- **Local-first architecture** - All data stored locally in SQLite
- **Full-text search** - SQLite FTS5-powered search across all messages
- **Modern TUI** - Terminal user interface built with Ratatui
- **Comprehensive CLI** - 13 commands for complete control

#### Commands
- `import` - Import conversations from LLM providers
- `list` - List all conversations with filtering
- `show` - Display conversation details
- `search` - Full-text search across messages
- `delete` - Remove conversations
- `export` - Export conversations to JSON
- `stats` - Show database statistics
- `validate` - Validate database integrity
- `backup` - Create database backup
- `restore` - Restore from backup
- `init` - Initialize database
- `tui` - Launch terminal UI
- `version` - Show version information

#### Crates
- `llm-unify-core` - Core types, traits, and domain models
- `llm-unify-storage` - SQLite persistence layer with migrations
- `llm-unify-parser` - Import parsers for all major providers
- `llm-unify-search` - Full-text search engine
- `llm-unify-cli` - Command-line interface
- `llm-unify-tui` - Terminal user interface

#### Documentation
- `README.adoc` - Project overview and quick start
- `SECURITY.md` - RFC 9116 compliant security policy
- `CONTRIBUTING.md` - Contribution guidelines with TPCF Perimeter 3
- `CODE_OF_CONDUCT.md` - Contributor Covenant 2.1 + CCCP extensions
- `MAINTAINERS.md` - Governance model and maintainer list
- `CHANGELOG.md` - Release history (this file)
- `RSR-COMPLIANCE.md` - Rhodium Standard Repository compliance report

#### .well-known Directory
- `security.txt` - RFC 9116 security contact information
- `ai.txt` - AI training policies and ethical guidelines
- `humans.txt` - Human attribution and credits

#### Build System
- `justfile` - Task automation with RSR validation recipes
- `.gitlab-ci.yml` - CI/CD pipeline with 6 stages

#### Security
- **Zero unsafe blocks** - Complete memory safety via Rust
- **Parameterized queries** - SQL injection prevention
- **Input validation** - File path and JSON sanitization
- **Dependency auditing** - Automated `cargo audit` in CI

### Changed
- N/A (initial release)

### Deprecated
- N/A (initial release)

### Removed
- N/A (initial release)

### Fixed
- N/A (initial release)

### Security
- Established security reporting process (security@llm-unify.dev)
- Documented known limitations (no database encryption in v0.1)
- Set up automated dependency scanning

## RSR Compliance

**Current Level:** Silver (51/55 points)

| Category | Score | Level |
|----------|-------|-------|
| Documentation | 5/5 | Gold |
| Type Safety | 5/5 | Gold |
| Memory Safety | 5/5 | Gold |
| Offline-First | 5/5 | Gold |
| .well-known/ | 5/5 | Gold |
| Build System | 5/5 | Gold |
| Test Coverage | 3/5 | Bronze |
| TPCF | 5/5 | Gold |
| License | 5/5 | Gold |
| Community | 4/5 | Silver |
| Security | 4/5 | Silver |

See [RSR-COMPLIANCE.md](RSR-COMPLIANCE.md) for detailed compliance report.

---

## Version History Summary

- **v0.1.0** (2025-11-28) - Initial release with RSR Silver compliance

## Links

- [Repository](https://github.com/Hyperpolymath/llm-unify)
- [Issue Tracker](https://github.com/Hyperpolymath/llm-unify/issues)
- [Security Policy](SECURITY.md)
- [Contributing Guide](CONTRIBUTING.md)
