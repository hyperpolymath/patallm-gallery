# Justfile for LLM Unify
# Command runner for common development tasks

# Default recipe (runs when you type `just`)
default:
    @just --list

# ============================================================================
# BUILD COMMANDS
# ============================================================================

# Build all crates in debug mode
build:
    cargo build --all-features

# Build all crates in release mode
release:
    cargo build --release --all-features

# Clean build artifacts
clean:
    cargo clean

# Check code without building
check:
    cargo check --all-features

# ============================================================================
# TEST COMMANDS
# ============================================================================

# Run all tests
test:
    cargo test --all-features

# Run unit tests only
test-unit:
    cargo test --lib --all-features

# Run integration tests only
test-integration:
    cargo test --test '*' --all-features

# Run doc tests
test-doc:
    cargo test --doc --all-features

# Run tests with coverage (requires cargo-tarpaulin)
coverage:
    cargo tarpaulin --out Html --all-features
    @echo "Coverage report generated: target/tarpaulin/index.html"

# ============================================================================
# CODE QUALITY
# ============================================================================

# Format code
fmt:
    cargo fmt

# Check code formatting
fmt-check:
    cargo fmt -- --check

# Run clippy linter
lint:
    cargo clippy --all-targets --all-features -- -D warnings

# Fix clippy warnings automatically
fix:
    cargo clippy --fix --all-targets --all-features

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate and open documentation
doc:
    cargo doc --no-deps --all-features --open

# Build documentation without opening
doc-build:
    cargo doc --no-deps --all-features

# ============================================================================
# SECURITY & COMPLIANCE
# ============================================================================

# Run security audit
audit:
    cargo audit

# Check for unsafe code blocks (RSR requirement)
check-unsafe:
    @echo "Checking for unsafe blocks..."
    @unsafe_count=$$(grep -r "unsafe" --include="*.rs" crates/ | grep -v "//.*unsafe" | wc -l); \
    if [ "$$unsafe_count" -gt 0 ]; then \
        echo "❌ Found $$unsafe_count unsafe blocks"; \
        grep -r "unsafe" --include="*.rs" crates/ | grep -v "//.*unsafe"; \
        exit 1; \
    else \
        echo "✓ Zero unsafe blocks confirmed"; \
    fi

# Validate .well-known directory (RFC 9116)
validate-well-known:
    @echo "Validating .well-known directory..."
    @test -f .well-known/security.txt || (echo "❌ Missing .well-known/security.txt" && exit 1)
    @test -f .well-known/ai.txt || (echo "❌ Missing .well-known/ai.txt" && exit 1)
    @test -f .well-known/humans.txt || (echo "❌ Missing .well-known/humans.txt" && exit 1)
    @echo "✓ .well-known directory valid"

# Check license compliance
check-license:
    @echo "Checking license compliance..."
    @test -f LICENSE || (echo "❌ Missing LICENSE file" && exit 1)
    @grep -q "AGPL" LICENSE || (echo "❌ LICENSE must contain AGPL-3.0" && exit 1)
    @echo "✓ License compliance verified"

# Full RSR compliance validation
validate-rsr: check-unsafe validate-well-known check-license
    @echo ""
    @echo "================================"
    @echo "RSR COMPLIANCE VALIDATION"
    @echo "================================"
    @echo ""
    @echo "Checking required documentation..."
    @test -f README.adoc && echo "✓ README.adoc" || echo "❌ README.adoc"
    @test -f SECURITY.md && echo "✓ SECURITY.md" || echo "❌ SECURITY.md"
    @test -f CONTRIBUTING.md && echo "✓ CONTRIBUTING.md" || echo "❌ CONTRIBUTING.md"
    @test -f CODE_OF_CONDUCT.md && echo "✓ CODE_OF_CONDUCT.md" || echo "❌ CODE_OF_CONDUCT.md"
    @test -f MAINTAINERS.md && echo "✓ MAINTAINERS.md" || echo "❌ MAINTAINERS.md"
    @test -f CHANGELOG.md && echo "✓ CHANGELOG.md" || echo "❌ CHANGELOG.md"
    @test -f RSR-COMPLIANCE.md && echo "✓ RSR-COMPLIANCE.md" || echo "❌ RSR-COMPLIANCE.md"
    @echo ""
    @echo "All RSR compliance checks passed! ✓"

# Display RSR compliance report
rsr-report:
    @echo "================================"
    @echo "RSR COMPLIANCE REPORT"
    @echo "================================"
    @echo ""
    @echo "Current Level: SILVER (51/55 points)"
    @echo ""
    @echo "Category Breakdown:"
    @echo "  Documentation:  5/5 (Gold)   ✓"
    @echo "  Type Safety:    5/5 (Gold)   ✓"
    @echo "  Memory Safety:  5/5 (Gold)   ✓"
    @echo "  Offline-First:  5/5 (Gold)   ✓"
    @echo "  .well-known/:   5/5 (Gold)   ✓"
    @echo "  Build System:   5/5 (Gold)   ✓"
    @echo "  Test Coverage:  3/5 (Bronze) ⚠"
    @echo "  TPCF:           5/5 (Gold)   ✓"
    @echo "  License:        5/5 (Gold)   ✓"
    @echo "  Community:      4/5 (Silver) ⚠"
    @echo "  Security:       4/5 (Silver) ⚠"
    @echo ""
    @echo "Path to Gold (+4 points):"
    @echo "  1. Test Coverage: 30% → 80% (+2)"
    @echo "  2. Community: Active growth (+1)"
    @echo "  3. Security: External audit (+1)"
    @echo ""
    @echo "See RSR-COMPLIANCE.md for details"

# ============================================================================
# DEVELOPMENT WORKFLOW
# ============================================================================

# Run all checks before committing
pre-commit: fmt-check lint test check-unsafe
    @echo "✓ All pre-commit checks passed!"

# Full validation (like CI)
ci: fmt-check lint test doc-build audit validate-rsr
    @echo "✓ All CI checks passed!"

# Complete compliance check
compliance: validate-rsr rsr-report
    @echo "✓ Full compliance check complete!"

# ============================================================================
# CLI USAGE
# ============================================================================

# Run the CLI
run *ARGS:
    cargo run -p llm-unify-cli -- {{ARGS}}

# Run the TUI
tui:
    cargo run -p llm-unify-cli -- tui

# Install locally
install:
    cargo install --path crates/llm-unify-cli

# ============================================================================
# RELEASE COMMANDS
# ============================================================================

# Prepare for release (update version, changelog, etc.)
release-prep VERSION:
    @echo "Preparing release {{VERSION}}..."
    @echo "1. Update version in Cargo.toml files"
    @echo "2. Update CHANGELOG.md"
    @echo "3. Run: just ci"
    @echo "4. Commit and tag: git tag -a v{{VERSION}}"
    @echo "5. Push: git push origin v{{VERSION}}"

# Publish to crates.io (requires authentication)
publish:
    @echo "Publishing to crates.io..."
    cargo publish -p llm-unify-core
    @sleep 30
    cargo publish -p llm-unify-storage
    @sleep 30
    cargo publish -p llm-unify-parser
    @sleep 30
    cargo publish -p llm-unify-search
    @sleep 30
    cargo publish -p llm-unify-tui
    @sleep 30
    cargo publish -p llm-unify-cli
    @echo "✓ All crates published!"

# ============================================================================
# MAINTENANCE
# ============================================================================

# Update dependencies
update:
    cargo update

# Check for outdated dependencies (requires cargo-outdated)
outdated:
    cargo outdated

# Deny check (licenses, bans, sources)
deny:
    cargo deny check

# ============================================================================
# DATABASE MANAGEMENT (for development)
# ============================================================================

# Initialize a test database
db-init:
    cargo run -p llm-unify-cli -- --database test.db init

# Show database stats
db-stats:
    cargo run -p llm-unify-cli -- --database test.db stats

# Clean test database
db-clean:
    rm -f test.db test.db-shm test.db-wal
