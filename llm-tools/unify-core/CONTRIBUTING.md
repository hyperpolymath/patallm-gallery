# Contributing to LLM Unify Core

Thank you for considering contributing to LLM Unify Core! This document outlines the
guidelines and processes for contributing.

## Versioning Policy

This project follows [Semantic Versioning 2.0.0](https://semver.org/).

### Version Format: MAJOR.MINOR.PATCH

- **MAJOR**: Incompatible API changes
- **MINOR**: New functionality (backwards-compatible)
- **PATCH**: Bug fixes (backwards-compatible)

### Pre-1.0 Development

While the version is below 1.0.0:
- MINOR bumps may include breaking changes
- PATCH bumps are for bug fixes only
- API stability is not guaranteed

## Commit Message Convention

All commits **must** follow [Conventional Commits](https://www.conventionalcommits.org/).

### Format

```
type(scope)?: description

[optional body]

[optional footer(s)]
```

### Types

| Type | Description | Version Bump |
|------|-------------|--------------|
| `feat` | New feature | MINOR |
| `fix` | Bug fix | PATCH |
| `docs` | Documentation only | none |
| `style` | Code style (formatting, etc.) | none |
| `refactor` | Code change without fix/feature | none |
| `perf` | Performance improvement | PATCH |
| `test` | Adding/updating tests | none |
| `build` | Build system changes | none |
| `ci` | CI configuration changes | none |
| `chore` | Maintenance tasks | none |
| `revert` | Revert previous commit | varies |

### Breaking Changes

For breaking changes, add `!` after the type or include `BREAKING CHANGE:` in footer:

```
feat!: remove deprecated provider API

BREAKING CHANGE: The old provider API has been removed.
Use ProviderTrait instead.
```

### Examples

```
feat(provider): add Gemini provider support
fix(models): correct message serialization order
docs: update installation instructions
chore(deps): bump serde to 1.0.195
refactor!: restructure module hierarchy
```

## Changelog Maintenance

We maintain a [CHANGELOG.md](CHANGELOG.md) following [Keep a Changelog](https://keepachangelog.com/).

### Adding Changelog Entries

When making changes:

1. Add entries under `[Unreleased]` section
2. Use appropriate category: Added, Changed, Deprecated, Removed, Fixed, Security
3. Write user-focused descriptions

### Release Process

Releases are created via the `release.yml` GitHub workflow:

1. Ensure all changes are documented in CHANGELOG.md under `[Unreleased]`
2. Run the Release workflow with the target version
3. The workflow will:
   - Validate the version format
   - Run all tests
   - Update version in Cargo.toml and lib.rs
   - Move `[Unreleased]` entries to the new version
   - Create a git tag
   - Publish a GitHub release

## Code Standards

### Language Policy

See [.claude/CLAUDE.md](.claude/CLAUDE.md) for the complete language policy.

**Key points:**
- Primary language: **Rust**
- No TypeScript, Node.js, or Go
- Use Deno for any JavaScript tooling needs

### Rust Guidelines

- Run `cargo fmt` before committing
- Ensure `cargo clippy` passes without warnings
- All public APIs must be documented
- Tests are required for new functionality

## Pull Request Process

1. Fork the repository
2. Create a feature branch from `main`
3. Make your changes following the guidelines above
4. Ensure all CI checks pass
5. Submit a pull request with a clear description

## License

By contributing, you agree that your contributions will be licensed under the
project's dual license: MIT OR AGPL-3.0-or-later.
