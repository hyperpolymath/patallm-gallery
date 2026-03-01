# elegant-STATE justfile - trillion recipe edition
# Run `just --list` for available recipes

set shell := ["bash", "-uc"]
set dotenv-load := true
set positional-arguments := true

# ══════════════════════════════════════════════════════════════════════════════
# VARIABLES
# ══════════════════════════════════════════════════════════════════════════════

project := "elegant-state"
version := `grep '^version' Cargo.toml | head -1 | sed 's/.*"\(.*\)".*/\1/'`
target_dir := "target"
release_dir := target_dir / "release"
debug_dir := target_dir / "debug"
doc_dir := target_dir / "doc"
man_dir := "doc/man"
bin_name := "state-cli"

# Build profiles
profile := env_var_or_default("PROFILE", "dev")
features := env_var_or_default("FEATURES", "")
target := env_var_or_default("TARGET", "")

# Guix
guix_channel := "guix/elegant-state"
guix_manifest := "guix.scm"

# Nickel
nickel_dir := "config"
nickel_main := nickel_dir / "main.ncl"

# Database
db_path := env_var_or_default("STATE_DB", "~/.local/share/elegant-state/db")
test_db := "/tmp/elegant-state-test"

# Server
host := env_var_or_default("HOST", "127.0.0.1")
port := env_var_or_default("PORT", "4000")

# Colors
red := '\033[0;31m'
green := '\033[0;32m'
yellow := '\033[0;33m'
blue := '\033[0;34m'
purple := '\033[0;35m'
cyan := '\033[0;36m'
nc := '\033[0m'

# ══════════════════════════════════════════════════════════════════════════════
# DEFAULT & HELP
# ══════════════════════════════════════════════════════════════════════════════

# Show all available recipes
@default:
    just --list --unsorted

# Show detailed help for a recipe
[group('help')]
help recipe="":
    #!/usr/bin/env bash
    if [ -z "{{recipe}}" ]; then
        echo -e "{{cyan}}elegant-STATE{{nc}} v{{version}}"
        echo ""
        echo "Usage: just <recipe> [args...]"
        echo ""
        just --list
        echo ""
        echo "Run 'just help <recipe>' for detailed help on a specific recipe."
    else
        just --show "{{recipe}}" 2>/dev/null || echo "Recipe '{{recipe}}' not found"
    fi

# Show version info
[group('help')]
version:
    @echo -e "{{cyan}}elegant-STATE{{nc}} v{{version}}"
    @echo "Rust: $(rustc --version)"
    @echo "Cargo: $(cargo --version)"
    @command -v guix >/dev/null && echo "Guix: $(guix --version | head -1)" || true
    @command -v nickel >/dev/null && echo "Nickel: $(nickel --version)" || true

# ══════════════════════════════════════════════════════════════════════════════
# BUILD RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Build in debug mode
[group('build')]
build *args:
    cargo build {{args}}

# Build in release mode
[group('build')]
build-release *args:
    cargo build --release {{args}}

# Build with all features
[group('build')]
build-all-features:
    cargo build --all-features

# Build with no default features
[group('build')]
build-minimal:
    cargo build --no-default-features

# Build for specific target
[group('build')]
build-target target *args:
    cargo build --target {{target}} {{args}}

# Build optimized for size
[group('build')]
build-small:
    RUSTFLAGS="-C opt-level=z -C lto=fat -C codegen-units=1" cargo build --release

# Build with debug symbols in release
[group('build')]
build-release-debug:
    cargo build --release --config 'profile.release.debug=true'

# Cross-compile for multiple targets
[group('build')]
build-cross:
    #!/usr/bin/env bash
    targets=(
        "x86_64-unknown-linux-gnu"
        "x86_64-unknown-linux-musl"
        "aarch64-unknown-linux-gnu"
        "x86_64-apple-darwin"
        "aarch64-apple-darwin"
    )
    for t in "${targets[@]}"; do
        echo -e "{{blue}}Building for $t{{nc}}"
        cargo build --release --target "$t" || echo "Skipping $t (not installed)"
    done

# Build and copy to local bin
[group('build')]
install-local: build-release
    mkdir -p ~/.local/bin
    cp {{release_dir}}/{{bin_name}} ~/.local/bin/

# ══════════════════════════════════════════════════════════════════════════════
# TEST RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Run all tests
[group('test')]
test *args:
    cargo test {{args}}

# Run tests with output
[group('test')]
test-verbose:
    cargo test -- --nocapture

# Run tests in release mode
[group('test')]
test-release:
    cargo test --release

# Run specific test
[group('test')]
test-one name:
    cargo test {{name}} -- --nocapture

# Run tests matching pattern
[group('test')]
test-filter pattern:
    cargo test {{pattern}}

# Run tests with coverage (requires cargo-llvm-cov)
[group('test')]
test-coverage:
    cargo llvm-cov --html --output-dir {{target_dir}}/coverage
    @echo "Coverage report: {{target_dir}}/coverage/html/index.html"

# Run integration tests only
[group('test')]
test-integration:
    cargo test --test '*' -- --test-threads=1

# Run doc tests only
[group('test')]
test-doc:
    cargo test --doc

# Run tests with miri (requires nightly + miri)
[group('test')]
test-miri:
    cargo +nightly miri test

# Run property-based tests (if any)
[group('test')]
test-proptest:
    cargo test proptest --features proptest

# Benchmark tests
[group('test')]
bench *args:
    cargo bench {{args}}

# ══════════════════════════════════════════════════════════════════════════════
# LINT & FORMAT RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Run all lints
[group('lint')]
lint: lint-clippy lint-fmt-check lint-doc

# Run clippy
[group('lint')]
lint-clippy:
    cargo clippy --all-targets --all-features -- -D warnings

# Run clippy with fixes
[group('lint')]
lint-fix:
    cargo clippy --fix --allow-dirty --allow-staged

# Check formatting
[group('lint')]
lint-fmt-check:
    cargo fmt --check

# Format code
[group('lint')]
fmt:
    cargo fmt

# Format and lint
[group('lint')]
tidy: fmt lint-fix

# Check documentation
[group('lint')]
lint-doc:
    RUSTDOCFLAGS="-D warnings" cargo doc --no-deps

# Run cargo audit
[group('lint')]
audit:
    cargo audit

# Run cargo deny
[group('lint')]
deny:
    cargo deny check

# Run cargo outdated
[group('lint')]
outdated:
    cargo outdated

# Run all checks (CI-style)
[group('lint')]
check-all: lint test test-doc
    cargo check --all-targets --all-features

# ══════════════════════════════════════════════════════════════════════════════
# DOCUMENTATION RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Generate Rust docs
[group('docs')]
doc:
    cargo doc --no-deps --document-private-items

# Generate and open docs
[group('docs')]
doc-open:
    cargo doc --no-deps --open

# Generate man pages
[group('docs')]
doc-man:
    #!/usr/bin/env bash
    mkdir -p {{man_dir}}/man1
    # Generate from help2man if available
    if command -v help2man &>/dev/null; then
        cargo build --release
        help2man {{release_dir}}/{{bin_name}} > {{man_dir}}/man1/{{bin_name}}.1
    else
        echo "help2man not found, using asciidoctor"
        asciidoctor -b manpage -D {{man_dir}}/man1 doc/man/*.adoc
    fi

# Generate HTML docs from AsciiDoc
[group('docs')]
doc-html:
    #!/usr/bin/env bash
    mkdir -p {{doc_dir}}/html
    for f in *.adoc doc/*.adoc; do
        [ -f "$f" ] && asciidoctor -D {{doc_dir}}/html "$f"
    done

# Generate PDF docs
[group('docs')]
doc-pdf:
    #!/usr/bin/env bash
    mkdir -p {{doc_dir}}/pdf
    for f in *.adoc doc/*.adoc; do
        [ -f "$f" ] && asciidoctor-pdf -D {{doc_dir}}/pdf "$f"
    done

# Generate all docs
[group('docs')]
doc-all: doc doc-man doc-html

# ══════════════════════════════════════════════════════════════════════════════
# RUN RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Run the CLI with args
[group('run')]
run *args:
    cargo run -- {{args}}

# Run in release mode
[group('run')]
run-release *args:
    cargo run --release -- {{args}}

# Start GraphQL server
[group('run')]
serve:
    cargo run --release -- serve http --host {{host}} --port {{port}}

# Start server in background
[group('run')]
serve-bg:
    cargo run --release -- serve http --host {{host}} --port {{port}} &
    @echo "Server started on http://{{host}}:{{port}}/graphql"

# Run with tracing
[group('run')]
run-trace *args:
    RUST_LOG=debug cargo run -- {{args}}

# Run with full trace
[group('run')]
run-trace-full *args:
    RUST_LOG=trace cargo run -- {{args}}

# ══════════════════════════════════════════════════════════════════════════════
# CLI OPERATION RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Create a node
[group('cli')]
node-create kind content:
    cargo run --release -- node create --kind {{kind}} --content '{{content}}'

# List nodes
[group('cli')]
node-list kind="" limit="20":
    #!/usr/bin/env bash
    args="--limit {{limit}}"
    [ -n "{{kind}}" ] && args="--kind {{kind}} $args"
    cargo run --release -- node list $args

# Get a node
[group('cli')]
node-get id:
    cargo run --release -- node get {{id}}

# Create an edge
[group('cli')]
edge-create from to kind:
    cargo run --release -- edge create --from {{from}} --to {{to}} --kind {{kind}}

# Search nodes
[group('cli')]
search query kinds="":
    #!/usr/bin/env bash
    args="{{query}}"
    [ -n "{{kinds}}" ] && args="$args --kinds {{kinds}}"
    cargo run --release -- search $args

# Show events
[group('cli')]
events limit="20":
    cargo run --release -- events --limit {{limit}}

# Export state
[group('cli')]
export format="json":
    cargo run --release -- export --format {{format}}

# Import state
[group('cli')]
import file:
    cargo run --release -- import {{file}}

# ══════════════════════════════════════════════════════════════════════════════
# DATABASE RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Initialize a fresh database
[group('db')]
db-init:
    #!/usr/bin/env bash
    mkdir -p "$(dirname {{db_path}})"
    echo "Database initialized at {{db_path}}"

# Reset database (destructive!)
[group('db')]
db-reset:
    #!/usr/bin/env bash
    read -p "This will DELETE all data. Are you sure? [y/N] " confirm
    if [ "$confirm" = "y" ]; then
        rm -rf {{db_path}}
        just db-init
        echo "Database reset complete"
    fi

# Backup database
[group('db')]
db-backup name=`date +%Y%m%d-%H%M%S`:
    #!/usr/bin/env bash
    backup_dir="backups"
    mkdir -p "$backup_dir"
    cp -r {{db_path}} "$backup_dir/{{name}}"
    echo "Backed up to $backup_dir/{{name}}"

# Restore database
[group('db')]
db-restore name:
    #!/usr/bin/env bash
    backup="backups/{{name}}"
    if [ -d "$backup" ]; then
        rm -rf {{db_path}}
        cp -r "$backup" {{db_path}}
        echo "Restored from $backup"
    else
        echo "Backup not found: $backup"
        exit 1
    fi

# Show database stats
[group('db')]
db-stats:
    @du -sh {{db_path}} 2>/dev/null || echo "No database found"

# Compact database
[group('db')]
db-compact:
    @echo "Compacting database..."
    cargo run --release -- db compact 2>/dev/null || echo "Compact not implemented yet"

# ══════════════════════════════════════════════════════════════════════════════
# GUIX RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Enter Guix dev shell
[group('guix')]
guix-shell:
    guix shell -m {{guix_manifest}}

# Build with Guix
[group('guix')]
guix-build:
    guix build -f {{guix_channel}}/packages.scm

# Build container image
[group('guix')]
guix-container:
    guix pack -f docker -S /bin=bin elegant-state

# Build Singularity image
[group('guix')]
guix-singularity:
    guix pack -f squashfs elegant-state

# Update Guix channel
[group('guix')]
guix-pull:
    guix pull

# Check Guix package
[group('guix')]
guix-lint:
    guix lint -c {{guix_channel}}/packages.scm

# Show Guix package info
[group('guix')]
guix-show:
    guix show elegant-state

# ══════════════════════════════════════════════════════════════════════════════
# NICKEL CONFIG RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Evaluate Nickel config
[group('nickel')]
nickel-eval *args:
    nickel eval {{nickel_main}} {{args}}

# Export config as JSON
[group('nickel')]
nickel-json:
    nickel export {{nickel_main}}

# Export config as TOML
[group('nickel')]
nickel-toml:
    nickel export --format toml {{nickel_main}}

# Export config as YAML
[group('nickel')]
nickel-yaml:
    nickel export --format yaml {{nickel_main}}

# Type-check Nickel config
[group('nickel')]
nickel-check:
    nickel typecheck {{nickel_main}}

# Format Nickel files
[group('nickel')]
nickel-fmt:
    nickel format {{nickel_dir}}/*.ncl

# Generate config for environment
[group('nickel')]
config env="dev":
    nickel export {{nickel_dir}}/{{env}}.ncl > config.{{env}}.json

# Generate all config permutations
[group('nickel')]
config-matrix:
    #!/usr/bin/env bash
    mkdir -p generated-configs
    for env in dev staging prod; do
        for mode in direct proposal; do
            for strategy in unanimous majority supermajority; do
                nickel export --field "env=\"$env\" & mode=\"$mode\" & strategy=\"$strategy\"" \
                    {{nickel_main}} > "generated-configs/${env}-${mode}-${strategy}.json"
            done
        done
    done
    echo "Generated $(ls generated-configs | wc -l) config permutations"

# ══════════════════════════════════════════════════════════════════════════════
# RELEASE RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Create release build
[group('release')]
release: lint test build-release doc-man
    @echo -e "{{green}}Release build complete{{nc}}"

# Create release tarball
[group('release')]
release-tar: release
    #!/usr/bin/env bash
    name="{{project}}-{{version}}"
    mkdir -p "dist/$name"
    cp {{release_dir}}/{{bin_name}} "dist/$name/"
    cp README.adoc LICENSE.txt "dist/$name/"
    cp -r {{man_dir}} "dist/$name/"
    cd dist && tar -czvf "$name.tar.gz" "$name"
    echo -e "{{green}}Created dist/$name.tar.gz{{nc}}"

# Create release for all platforms
[group('release')]
release-all: build-cross
    #!/usr/bin/env bash
    for dir in {{target_dir}}/*/release; do
        target=$(basename $(dirname $dir))
        if [ -f "$dir/{{bin_name}}" ]; then
            name="{{project}}-{{version}}-$target"
            mkdir -p "dist/$name"
            cp "$dir/{{bin_name}}" "dist/$name/"
            cd dist && tar -czvf "$name.tar.gz" "$name" && cd ..
        fi
    done

# Bump version
[group('release')]
bump-version level="patch":
    #!/usr/bin/env bash
    current="{{version}}"
    IFS='.' read -ra parts <<< "$current"
    major=${parts[0]}
    minor=${parts[1]}
    patch=${parts[2]}
    case "{{level}}" in
        major) major=$((major + 1)); minor=0; patch=0 ;;
        minor) minor=$((minor + 1)); patch=0 ;;
        patch) patch=$((patch + 1)) ;;
    esac
    new="$major.$minor.$patch"
    sed -i "s/version = \"$current\"/version = \"$new\"/" Cargo.toml
    echo "Bumped version: $current -> $new"

# Tag release
[group('release')]
tag:
    git tag -a "v{{version}}" -m "Release v{{version}}"
    git push origin "v{{version}}"

# ══════════════════════════════════════════════════════════════════════════════
# CI/CD RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Full CI pipeline
[group('ci')]
ci: check-all test-coverage doc-all
    @echo -e "{{green}}CI pipeline complete{{nc}}"

# Quick CI (no coverage)
[group('ci')]
ci-quick: lint test doc
    @echo -e "{{green}}Quick CI complete{{nc}}"

# Pre-commit hook
[group('ci')]
pre-commit: fmt lint-clippy test
    @echo -e "{{green}}Pre-commit checks passed{{nc}}"

# Pre-push hook
[group('ci')]
pre-push: check-all
    @echo -e "{{green}}Pre-push checks passed{{nc}}"

# Install git hooks
[group('ci')]
install-hooks:
    #!/usr/bin/env bash
    mkdir -p .git/hooks
    echo '#!/bin/sh
    just pre-commit' > .git/hooks/pre-commit
    chmod +x .git/hooks/pre-commit
    echo '#!/bin/sh
    just pre-push' > .git/hooks/pre-push
    chmod +x .git/hooks/pre-push
    echo "Git hooks installed"

# ══════════════════════════════════════════════════════════════════════════════
# CLEAN RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Clean build artifacts
[group('clean')]
clean:
    cargo clean

# Clean everything including generated files
[group('clean')]
clean-all: clean
    rm -rf dist generated-configs backups
    rm -rf {{doc_dir}}/html {{doc_dir}}/pdf

# Clean test databases
[group('clean')]
clean-test:
    rm -rf {{test_db}}

# ══════════════════════════════════════════════════════════════════════════════
# DEVELOPMENT RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Watch and rebuild on changes
[group('dev')]
watch:
    cargo watch -x build

# Watch and test on changes
[group('dev')]
watch-test:
    cargo watch -x test

# Watch and run on changes
[group('dev')]
watch-run *args:
    cargo watch -x "run -- {{args}}"

# Start development environment
[group('dev')]
dev: db-init
    #!/usr/bin/env bash
    echo -e "{{cyan}}Starting development environment{{nc}}"
    echo "Database: {{db_path}}"
    echo "Run 'just serve' to start the GraphQL server"
    echo "Run 'just watch' for auto-rebuild"

# Generate shell completions
[group('dev')]
completions shell="bash":
    #!/usr/bin/env bash
    cargo run --release -- completions {{shell}} > completions.{{shell}}
    echo "Generated completions.{{shell}}"

# Generate completions for all shells
[group('dev')]
completions-all:
    just completions bash
    just completions zsh
    just completions fish
    just completions elvish
    just completions powershell

# ══════════════════════════════════════════════════════════════════════════════
# GRAPHQL RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Export GraphQL schema
[group('graphql')]
graphql-schema:
    cargo run --release -- graphql schema > schema.graphql

# Start GraphQL playground
[group('graphql')]
graphql-playground:
    @echo "GraphQL Playground: http://{{host}}:{{port}}/playground"
    just serve

# Run GraphQL query from file
[group('graphql')]
graphql-query file:
    #!/usr/bin/env bash
    query=$(cat {{file}})
    curl -s -X POST http://{{host}}:{{port}}/graphql \
        -H "Content-Type: application/json" \
        -d "{\"query\": \"$query\"}" | jq

# Introspect GraphQL API
[group('graphql')]
graphql-introspect:
    #!/usr/bin/env bash
    curl -s -X POST http://{{host}}:{{port}}/graphql \
        -H "Content-Type: application/json" \
        -d '{"query": "{ __schema { types { name } } }"}' | jq

# ══════════════════════════════════════════════════════════════════════════════
# DOCKER/CONTAINER RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Build container image
[group('container')]
container-build:
    docker build -t {{project}}:{{version}} .

# Run container
[group('container')]
container-run:
    docker run -p {{port}}:{{port}} {{project}}:{{version}}

# Push container to registry
[group('container')]
container-push registry:
    docker tag {{project}}:{{version}} {{registry}}/{{project}}:{{version}}
    docker push {{registry}}/{{project}}:{{version}}

# ══════════════════════════════════════════════════════════════════════════════
# UTILITY RECIPES
# ══════════════════════════════════════════════════════════════════════════════

# Count lines of code
[group('util')]
loc:
    @tokei --exclude target --exclude node_modules

# Show dependency tree
[group('util')]
deps:
    cargo tree

# Show unused dependencies
[group('util')]
deps-unused:
    cargo +nightly udeps

# Update dependencies
[group('util')]
deps-update:
    cargo update

# Generate flamegraph (requires cargo-flamegraph)
[group('util')]
flamegraph *args:
    cargo flamegraph -- {{args}}

# Profile with perf
[group('util')]
perf *args:
    perf record --call-graph dwarf cargo run --release -- {{args}}
    perf report

# Check for security vulnerabilities
[group('util')]
security: audit deny
    @echo -e "{{green}}Security checks passed{{nc}}"

# Print all recipes as markdown
[group('util')]
recipes-md:
    #!/usr/bin/env bash
    echo "# Available Recipes"
    echo ""
    just --list --unsorted | while read line; do
        if [[ $line == *":"* ]]; then
            recipe=$(echo "$line" | cut -d: -f1 | xargs)
            desc=$(echo "$line" | cut -d: -f2- | xargs)
            echo "- \`$recipe\`: $desc"
        fi
    done
