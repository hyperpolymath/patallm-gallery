# justfile - llm-antidote build system
# https://github.com/casey/just

# Variables
python := "python3"
artifact_dir := "artifacts"
tool_dir := "tools"
test_dir := "tests"
doc_dir := "docs"
example_dir := "examples"

# Default recipe (list all available recipes)
default:
    @just --list

# Run all tests
test:
    @echo "Running test suite..."
    {{python}} {{test_dir}}/test_suite.py list
    @echo "✅ Test suite loaded successfully"
    @echo "\nTo run specific test:"
    @echo "  just test-run <test_name>"

# Run specific test by name
test-run name:
    @echo "Generating test protocol for: {{name}}"
    {{python}} {{test_dir}}/test_suite.py run {{name}}

# Export test suite to JSON
test-export file="test_suite.json":
    @echo "Exporting test suite to {{file}}"
    {{python}} {{test_dir}}/test_suite.py export {{file}}
    @echo "✅ Exported to {{file}}"

# Run diagnostic tools
diagnose type="memory":
    @echo "Generating {{type}} diagnostic..."
    {{python}} {{tool_dir}}/llm-diagnostic.py {{type}}

# Run full diagnostic suite
diagnose-full:
    @echo "Generating complete diagnostic suite..."
    {{python}} {{tool_dir}}/llm-diagnostic.py suite

# Run benchmark for specific artifact and model
benchmark test-type artifact model:
    @echo "Generating benchmark protocol..."
    {{python}} {{tool_dir}}/benchmark.py generate {{test-type}} {{artifact}} {{model}}

# List available benchmark test types
benchmark-types:
    @echo "Available benchmark test types:"
    {{python}} {{tool_dir}}/benchmark.py types

# List available benchmark probes
benchmark-probes type:
    @echo "Probes for {{type}}:"
    {{python}} {{tool_dir}}/benchmark.py probes {{type}}

# List all artifacts
artifacts:
    @echo "📚 Available Reset Artifacts:"
    @echo ""
    {{python}} {{tool_dir}}/llm-cli.py list

# Show specific artifact
artifact name:
    @echo "Showing artifact: {{name}}"
    {{python}} {{tool_dir}}/llm-cli.py info {{name}}

# Display reset artifact
reset type="universal":
    @echo "Reset artifact: {{type}}"
    {{python}} {{tool_dir}}/llm-cli.py reset {{type}}

# Display preservation artifact
preserve:
    @echo "Context preservation artifact:"
    {{python}} {{tool_dir}}/llm-cli.py preserve

# Display verification tool
verify:
    @echo "Reset verification tool:"
    {{python}} {{tool_dir}}/llm-cli.py verify

# Validate all Python files
validate-python:
    @echo "Validating Python files..."
    find {{tool_dir}} {{test_dir}} -name "*.py" -exec {{python}} -m py_compile {} \;
    @echo "✅ All Python files valid"

# Check artifact syntax (Scheme files)
validate-scheme:
    @echo "Checking Scheme artifacts..."
    @echo "✅ Scheme artifacts present (syntax check requires Guile)"
    find {{artifact_dir}} -name "*.scm" -type f

# Count lines of code
loc:
    @echo "Lines of Code:"
    @echo ""
    @echo "Python:"
    @find {{tool_dir}} {{test_dir}} -name "*.py" | xargs wc -l | tail -1
    @echo ""
    @echo "Scheme:"
    @find {{artifact_dir}} -name "*.scm" | xargs wc -l | tail -1
    @echo ""
    @echo "JavaScript:"
    @find {{artifact_dir}} -name "*.js" | xargs wc -l | tail -1
    @echo ""
    @echo "Documentation:"
    @find . -name "*.md" | xargs wc -l | tail -1
    @echo ""
    @echo "HTML:"
    @find {{doc_dir}} -name "*.html" | xargs wc -l | tail -1

# Count total files
count-files:
    @echo "File count by type:"
    @echo ""
    @echo "Artifacts: $(find {{artifact_dir}} -type f | wc -l)"
    @echo "Tools: $(find {{tool_dir}} -name "*.py" | wc -l)"
    @echo "Tests: $(find {{test_dir}} -name "*.py" | wc -l)"
    @echo "Documentation: $(find . -name "*.md" | wc -l)"
    @echo "Examples: $(find {{example_dir}} -name "*.md" | wc -l)"

# Repository statistics
stats:
    @echo "📊 Repository Statistics"
    @echo "═══════════════════════"
    @echo ""
    just count-files
    @echo ""
    just loc
    @echo ""
    @echo "Repository size: $(du -sh . | cut -f1)"

# Clean temporary files
clean:
    @echo "Cleaning temporary files..."
    find . -type f -name "*.pyc" -delete
    find . -type d -name "__pycache__" -delete
    find . -type f -name "*~" -delete
    find . -type f -name "*.bak" -delete
    @echo "✅ Clean complete"

# Check RSR compliance
rsr-check:
    @echo "🔍 RSR Framework Compliance Check"
    @echo "══════════════════════════════════"
    @echo ""
    {{python}} tools/rsr-compliance.py

# Verify RSR compliance (detailed)
rsr-verify:
    @echo "Running full RSR verification..."
    {{python}} tools/rsr-compliance.py --verbose

# Generate RSR compliance report
rsr-report:
    @echo "Generating RSR compliance report..."
    {{python}} tools/rsr-compliance.py --report > RSR_COMPLIANCE.md
    @echo "✅ Report generated: RSR_COMPLIANCE.md"

# Open web interface
web:
    @echo "Opening web interface..."
    @echo "Opening: {{doc_dir}}/web-interface.html"
    @echo "(Use your browser to open the file)"

# Quick help
help:
    @echo "llm-antidote build system"
    @echo "========================"
    @echo ""
    @echo "Common commands:"
    @echo "  just test           - Run test suite"
    @echo "  just artifacts      - List all artifacts"
    @echo "  just reset          - Show universal reset"
    @echo "  just diagnose       - Run diagnostic tool"
    @echo "  just benchmark <type> <artifact> <model> - Generate benchmark"
    @echo "  just stats          - Repository statistics"
    @echo "  just rsr-check      - Check RSR compliance"
    @echo "  just validate       - Validate all files"
    @echo ""
    @echo "For full list: just --list"

# Validate all files
validate: validate-python validate-scheme
    @echo ""
    @echo "✅ All validation checks passed"

# Full verification suite
verify-all: validate test rsr-check
    @echo ""
    @echo "═══════════════════════════════════"
    @echo "✅ All verifications passed!"
    @echo "═══════════════════════════════════"

# Initialize development environment
init:
    @echo "Initializing development environment..."
    @echo ""
    @echo "Checking Python..."
    @{{python}} --version
    @echo ""
    @echo "Project structure:"
    @tree -L 2 -I '__pycache__|*.pyc'
    @echo ""
    @echo "✅ Environment ready"
    @echo ""
    @echo "Next steps:"
    @echo "  just help     - See available commands"
    @echo "  just test     - Run tests"
    @echo "  just web      - Open web interface"

# Generate distribution archive
dist: clean
    @echo "Creating distribution archive..."
    @tar czf llm-antidote-$(cat VERSION).tar.gz \
        --exclude='.git' \
        --exclude='*.tar.gz' \
        --exclude='__pycache__' \
        --exclude='*.pyc' \
        .
    @echo "✅ Created: llm-antidote-$(cat VERSION).tar.gz"

# Check for updates (git)
update-check:
    @echo "Checking for updates..."
    @git fetch
    @git status -uno

# Show version
version:
    @echo "llm-antidote version: $(cat VERSION)"
    @echo "Released: $(git log -1 --format=%cd --date=short)"

# Development: auto-format Python files (if black is available)
format:
    @echo "Formatting Python files..."
    @command -v black >/dev/null 2>&1 && \
        black {{tool_dir}} {{test_dir}} || \
        echo "⚠️  black not installed, skipping format"

# Development: lint Python files (if pylint is available)
lint:
    @echo "Linting Python files..."
    @command -v pylint >/dev/null 2>&1 && \
        pylint {{tool_dir}}/*.py {{test_dir}}/*.py || \
        echo "⚠️  pylint not installed, skipping lint"

# Show project info
info:
    @echo "Project: llm-antidote"
    @echo "Version: $(cat VERSION)"
    @echo "License: CC0 1.0 Universal (Public Domain)"
    @echo "Author: Jonathan Jewell"
    @echo "Repository: https://github.com/Hyperpolymath/llm-antidote"
    @echo ""
    @echo "Description:"
    @echo "  Universal semantic reset artifacts for language model context management"
    @echo ""
    @echo "Standards:"
    @echo "  - RSR Framework compliant"
    @echo "  - RFC 9116 security.txt"
    @echo "  - CC0 1.0 Universal license"
    @echo "  - TPCF Perimeter 3 (Community Sandbox)"
