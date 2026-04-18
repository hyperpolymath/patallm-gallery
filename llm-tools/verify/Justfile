# claude-verify build commands

# Default recipe
default: build

# Build the project
build:
    cabal build all

# Build with optimizations
build-release:
    cabal build all --enable-optimization=2

# Run tests
test:
    cabal test all --test-show-details=streaming

# Run tests with coverage
test-coverage:
    cabal test all --enable-coverage

# Install locally
install:
    cabal install --installdir=~/.local/bin --overwrite-policy=always

# Clean build artifacts
clean:
    cabal clean

# Format code
format:
    find src app test -name '*.hs' -exec ormolu -i {} \;

# Lint code
lint:
    hlint src app test

# Generate documentation
docs:
    cabal haddock --haddock-hyperlink-source

# Run in development mode (ghcid)
dev:
    ghcid --command='cabal repl lib:claude-verify'

# Run the CLI
run *ARGS:
    cabal run claude-verify -- {{ARGS}}

# Verify a file
verify *FILES:
    cabal run claude-verify -- verify {{FILES}}

# Initialize context file
init:
    cabal run claude-verify -- init

# Show feedback
feedback *ARGS:
    cabal run claude-verify -- feedback {{ARGS}}

# Check ECHIDNA availability
check-echidna:
    @which echidna || echo "ECHIDNA not found in PATH"
    @echidna --version 2>/dev/null || echo "Install from: https://github.com/Hyperpolymath/echidna"

# Set up pre-commit hook
setup-hook:
    echo '#!/bin/bash' > .git/hooks/pre-commit
    echo 'cabal run claude-verify -- check --staged --fail-on-error' >> .git/hooks/pre-commit
    chmod +x .git/hooks/pre-commit

# Update dependencies
update:
    cabal update
    cabal outdated

# Profile build
profile:
    cabal build all --enable-profiling
    cabal run claude-verify -- verify test/fixtures/*.rs +RTS -p

# Generate SBOM
sbom:
    cabal-plan license-report > sbom.spdx

# All checks (for CI)
ci: build test lint
    @echo "All checks passed"
