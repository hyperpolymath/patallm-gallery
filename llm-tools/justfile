# SPDX-License-Identifier: PMPL-1.0-or-later
# llm-tools unified justfile
# Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

# Default recipe: list available commands
default:
    @just --list

# Build all Rust components (unify + unify-core)
build-rust:
    cd unify && cargo build --release
    cd unify-core && cargo build --release

# Build the Haskell verifier
build-verify:
    cd verify && cabal build all

# Build everything
build: build-rust build-verify

# Run all tests
test: test-rust test-verify

# Run Rust tests
test-rust:
    cd unify && cargo test
    cd unify-core && cargo test

# Run Haskell tests
test-verify:
    cd verify && cabal test all

# Run Rust clippy lints
lint-rust:
    cd unify && cargo clippy -- -D warnings
    cd unify-core && cargo clippy -- -D warnings

# Format Rust code
fmt-rust:
    cd unify && cargo fmt
    cd unify-core && cargo fmt

# Check Rust formatting
fmt-check-rust:
    cd unify && cargo fmt -- --check
    cd unify-core && cargo fmt -- --check

# Clean all build artifacts
clean:
    cd unify && cargo clean
    cd unify-core && cargo clean
    cd verify && cabal clean

# Run fuzz tests (Rust)
fuzz component target="":
    cd {{component}} && cargo fuzz run {{target}}

# Show component status
status:
    @echo "=== antidote ==="
    @ls antidote/artifacts/ 2>/dev/null || echo "  No artifacts"
    @echo "=== unify ==="
    @cd unify && cargo check 2>&1 | tail -1
    @echo "=== unify-core ==="
    @cd unify-core && cargo check 2>&1 | tail -1
    @echo "=== verify ==="
    @cd verify && cabal check 2>&1 | tail -1
