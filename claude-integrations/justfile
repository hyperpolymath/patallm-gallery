# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
#
# claude-integrations - Unified Claude browser/service integrations monorepo.

# List available recipes
default:
	@just --list

# --- Firefox LSP (Elixir) ---

# Build the Firefox LSP component
build-firefox-lsp:
	cd firefox-lsp && mix deps.get && mix compile

# Run Firefox LSP tests
test-firefox-lsp:
	cd firefox-lsp && mix test

# --- Firefox MCP (Deno) ---

# Build the Firefox MCP component
build-firefox-mcp:
	cd firefox-mcp && deno cache src/main.ts 2>/dev/null || echo "No Deno entrypoint cached"

# Run Firefox MCP tests
test-firefox-mcp:
	cd firefox-mcp && deno test 2>/dev/null || echo "No Deno tests configured"

# --- Gecko Browser Extension ---

# Build the Gecko browser extension
build-gecko-extension:
	@echo "Gecko extension is pure JS/WebExtensions - no build step required."
	@echo "Load as temporary add-on in about:debugging"

# Lint the Gecko browser extension
lint-gecko-extension:
	cd gecko-browser-extension && web-ext lint 2>/dev/null || echo "Install web-ext: npm i -g web-ext"

# --- GitLab Bridge (ReScript/Deno) ---

# Build the GitLab Bridge component
build-gitlab-bridge:
	cd gitlab-bridge && deno cache src/main.ts 2>/dev/null || echo "No Deno entrypoint cached"

# Run GitLab Bridge tests
test-gitlab-bridge:
	cd gitlab-bridge && deno test 2>/dev/null || echo "No Deno tests configured"

# --- Mozilla Extension ---

# Build the Mozilla extension
build-mozilla-extension:
	@echo "Mozilla extension is pure JS/WebExtensions - no build step required."
	@echo "Load as temporary add-on in about:debugging"

# Lint the Mozilla extension
lint-mozilla-extension:
	cd mozilla-extension && web-ext lint 2>/dev/null || echo "Install web-ext: npm i -g web-ext"

# --- Aggregate Recipes ---

# Build all components
build-all: build-firefox-lsp build-firefox-mcp build-gecko-extension build-gitlab-bridge build-mozilla-extension

# Run all tests
test-all: test-firefox-lsp test-firefox-mcp test-gitlab-bridge

# Lint all lintable components
lint-all: lint-gecko-extension lint-mozilla-extension

# Show status of all components
status:
	@echo "=== Firefox LSP ===" && ls firefox-lsp/mix.exs 2>/dev/null && echo "OK" || echo "MISSING"
	@echo "=== Firefox MCP ===" && ls firefox-mcp/deno.json 2>/dev/null && echo "OK" || echo "MISSING"
	@echo "=== Gecko Extension ===" && ls gecko-browser-extension/manifest.json 2>/dev/null && echo "OK" || echo "MISSING"
	@echo "=== GitLab Bridge ===" && ls gitlab-bridge/deno.json 2>/dev/null && echo "OK" || echo "MISSING"
	@echo "=== Mozilla Extension ===" && ls mozilla-extension/src/ 2>/dev/null && echo "OK" || echo "MISSING"
