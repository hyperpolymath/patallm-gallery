# Contributing to Did You Actually Do That?

Thank you for your interest in contributing! This document provides guidelines for contributing to the project.

## Code of Conduct

Be respectful, constructive, and inclusive. We're all here to build better tools for AI accountability.

## Getting Started

1. Fork the repository
2. Clone your fork:
   ```sh
   git clone https://github.com/YOUR_USERNAME/did-you-actually-do-that
   cd did-you-actually-do-that
   ```
3. Build and test:
   ```sh
   cargo build
   cargo test
   ```

## Development Setup

### Prerequisites

- Rust 1.70+ (stable)
- Cargo

### Useful Commands

```sh
cargo fmt          # Format code
cargo clippy       # Lint
cargo test         # Run tests
cargo doc --open   # Generate and view docs
```

## Making Changes

### Branch Naming

- Features: `feat/description`
- Fixes: `fix/description`
- Docs: `docs/description`

### Commit Messages

Follow conventional commits:

```
feat: add HTTP reachability evidence type
fix: handle empty claim evidence array
docs: add examples for custom checkers
test: add proptest for Verdict serialization
```

### Pull Request Process

1. Ensure all tests pass
2. Update documentation if needed
3. Add tests for new functionality
4. Run `cargo fmt` and `cargo clippy`
5. Submit PR with clear description

## Adding New Evidence Types

To add a new evidence type:

1. Add variant to `EvidenceSpec` enum in `src/lib.rs`:
   ```rust
   EvidenceSpec::YourNewType { param1: String, param2: u32 },
   ```

2. Add verification logic in `Verifier::check_evidence`:
   ```rust
   EvidenceSpec::YourNewType { param1, param2 } => {
       // Your verification logic
       (Verdict::Confirmed, Some("Details".to_string()))
   }
   ```

3. Add display logic in `src/main.rs` `print_report`:
   ```rust
   EvidenceSpec::YourNewType { param1, .. } => format!("Your type: {}", param1),
   ```

4. Add tests
5. Update README.md with the new type

## Testing Guidelines

- Unit tests go in the same file as the code (in `#[cfg(test)]` module)
- Integration tests go in `tests/` directory
- Property-based tests use `proptest`
- Aim for meaningful coverage, not 100%

### Running Specific Tests

```sh
cargo test test_name           # Single test
cargo test verdict             # Tests matching "verdict"
cargo test -- --nocapture      # Show println! output
```

## Documentation

- All public items need doc comments
- Examples in doc comments should compile (`cargo test --doc`)
- Update README.md for user-facing changes

## Questions?

Open an issue with the `question` label or reach out to maintainers.

## License

By contributing, you agree that your contributions will be licensed under MPL-2.0.

---

**Remember**: Please ensure any claimed changes are... actually made. 😉
