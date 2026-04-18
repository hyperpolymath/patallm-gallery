# Claude GitLab Bridge - Build Automation
# https://github.com/casey/just

# Default recipe (list all recipes)
default:
    @just --list

# === Development ===

# Install dependencies
install:
    npm install

# Build TypeScript sources
build:
    npm run build

# Run in development mode
dev:
    npm run dev

# Clean build artifacts
clean:
    npm run clean
    rm -rf node_modules/.cache
    rm -rf .turbo

# Full clean (includes node_modules)
clean-all: clean
    rm -rf node_modules/
    rm -f package-lock.json

# === Testing ===

# Run all tests
test:
    npm test

# Run tests in watch mode
test-watch:
    npm run test:watch

# Run tests with UI
test-ui:
    npm run test:ui

# Generate coverage report
test-coverage:
    npm run test:coverage
    @echo "Coverage report: coverage/index.html"

# === Quality ===

# Run linter
lint:
    npm run lint

# Fix linting issues
lint-fix:
    npm run lint:fix

# Format code with Prettier
format:
    npm run format

# Check formatting
format-check:
    npm run format:check

# Type check TypeScript
type-check:
    npm run type-check

# === Verification ===

# Comprehensive RSR GOLD validation
validate: format-check lint type-check test
    @echo ""
    @echo "=== RSR GOLD Compliance Validation ==="
    @echo ""
    @echo "Checking SPDX header on CLAUDE.md..."
    @if ! grep -q "SPDX-License-Identifier" CLAUDE.md; then \
        echo "❌ Missing SPDX header in CLAUDE.md"; \
        exit 1; \
    fi
    @echo "✓ CLAUDE.md has SPDX header"
    @echo ""
    @echo "Checking required files..."
    @test -f LICENSE.txt || (echo "❌ Missing LICENSE.txt" && exit 1)
    @test -f GOVERNANCE.adoc || (echo "❌ Missing GOVERNANCE.adoc" && exit 1)
    @test -f REVERSIBILITY.md || (echo "❌ Missing REVERSIBILITY.md" && exit 1)
    @test -f .gitattributes || (echo "❌ Missing .gitattributes" && exit 1)
    @test -f .well-known/security.txt || (echo "❌ Missing .well-known/security.txt" && exit 1)
    @test -f .well-known/ai.txt || (echo "❌ Missing .well-known/ai.txt" && exit 1)
    @test -f .well-known/humans.txt || (echo "❌ Missing .well-known/humans.txt" && exit 1)
    @test -f .well-known/consent-required.txt || (echo "❌ Missing .well-known/consent-required.txt" && exit 1)
    @test -f .well-known/provenance.json || (echo "❌ Missing .well-known/provenance.json" && exit 1)
    @test -f CODE_OF_CONDUCT.adoc || (echo "❌ Missing CODE_OF_CONDUCT.adoc" && exit 1)
    @test -f CONTRIBUTING.adoc || (echo "❌ Missing CONTRIBUTING.adoc" && exit 1)
    @echo "✓ All required RSR files present"
    @echo ""
    @echo "✓ RSR GOLD validation complete"

# Verify installation (run all checks)
verify: format-check lint type-check test
    @echo "✓ All verifications passed"

# Pre-commit checks
pre-commit: format lint type-check
    @echo "✓ Pre-commit checks passed"

# Pre-release checks
pre-release: clean build verify test-coverage
    @echo "✓ Ready for release"

# === Development ===

# Start application
start:
    npm start

# === Documentation ===

# Check documentation links
docs-check:
    @echo "Checking documentation for broken links..."
    @grep -r "](.*)" *.md docs/*.md 2>/dev/null | grep -v "http" || echo "✓ No obvious broken internal links"

# Count documentation lines
docs-stats:
    @echo "=== Documentation Statistics ==="
    @echo "Markdown files:"
    @find . -name "*.md" ! -path "./node_modules/*" -exec wc -l {} + | tail -1
    @echo "AsciiDoc files:"
    @find . -name "*.adoc" ! -path "./node_modules/*" -exec wc -l {} + | tail -1

# === Git ===

# Show git status
status:
    git status

# Show recent commits
log:
    git log --oneline -10

# Create new branch
branch name:
    git checkout -b {{name}}

# === Release ===

# Bump version (patch)
bump-patch:
    @echo "Current version: $(jq -r .version package.json)"
    @echo "Bumping patch version..."
    npm version patch --no-git-tag-version
    @echo "New version: $(jq -r .version package.json)"

# Bump version (minor)
bump-minor:
    @echo "Current version: $(jq -r .version package.json)"
    @echo "Bumping minor version..."
    npm version minor --no-git-tag-version
    @echo "New version: $(jq -r .version package.json)"

# Bump version (major)
bump-major:
    @echo "Current version: $(jq -r .version package.json)"
    @echo "Bumping major version..."
    npm version major --no-git-tag-version
    @echo "New version: $(jq -r .version package.json)"

# === Utilities ===

# Count lines of code
loc:
    @echo "=== Lines of Code ==="
    @echo "TypeScript:"
    @find src -name "*.ts" -exec wc -l {} + 2>/dev/null | tail -1 || echo "0"
    @echo "Tests:"
    @find tests -name "*.ts" -exec wc -l {} + 2>/dev/null | tail -1 || echo "0"
    @echo "Documentation:"
    @find . -name "*.md" ! -path "./node_modules/*" -exec wc -l {} + | tail -1

# Show project statistics
stats:
    @echo "=== Project Statistics ==="
    @echo "Total files: $(find . -type f ! -path "./.git/*" ! -path "./node_modules/*" | wc -l)"
    @echo "TypeScript files: $(find src -name "*.ts" 2>/dev/null | wc -l)"
    @echo "Test files: $(find tests -name "*.ts" 2>/dev/null | wc -l)"
    @echo "Documentation files: $(find . -name "*.md" -o -name "*.adoc" ! -path "./node_modules/*" | wc -l)"
    @just loc

# Show help for specific recipe
help recipe:
    @just --show {{recipe}}

# === Dependencies ===

# Update dependencies
update-deps:
    npm update

# Check for outdated dependencies
check-deps:
    npm outdated

# Audit dependencies for vulnerabilities
audit:
    npm audit

# Fix audit issues
audit-fix:
    npm audit fix

# === Environment ===

# Create .env from example
env-setup:
    @if [ ! -f .env ]; then \
        cp .env.example .env; \
        echo "✓ Created .env from .env.example"; \
        echo "⚠️  Please edit .env with your credentials"; \
    else \
        echo "⚠️  .env already exists"; \
    fi

# Validate .env file
env-check:
    @if [ ! -f .env ]; then \
        echo "❌ .env file not found"; \
        echo "Run: just env-setup"; \
        exit 1; \
    fi
    @echo "✓ .env file exists"
    @echo "Checking required variables..."
    @grep -q "GITLAB_TOKEN" .env || (echo "❌ Missing GITLAB_TOKEN" && exit 1)
    @grep -q "ANTHROPIC_API_KEY" .env || (echo "❌ Missing ANTHROPIC_API_KEY" && exit 1)
    @echo "✓ Required environment variables present"

# === Cleanup ===

# Remove all generated files
nuke: clean-all
    rm -rf coverage/
    rm -rf dist/
    rm -rf .turbo/
    @echo "✓ All generated files removed"

# === CI/CD ===

# Run CI checks
ci: install build lint type-check test
    @echo "✓ CI checks passed"

# Run full CI pipeline
ci-full: ci test-coverage validate
    @echo "✓ Full CI pipeline passed"
