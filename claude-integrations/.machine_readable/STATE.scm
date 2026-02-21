; SPDX-License-Identifier: PMPL-1.0-or-later
; STATE.scm - Current project state for claude-integrations

(state
  (metadata
    (version "1.0")
    (last-updated "2026-02-08")
    (updated-by "Claude Opus 4.6"))

  (project-context
    (name "claude-integrations")
    (type "monorepo")
    (description "Unified Claude browser/service integrations")
    (components
      ("firefox-lsp" "Elixir LSP for Firefox via Marionette")
      ("firefox-mcp" "MCP server for Firefox browser control")
      ("gecko-browser-extension" "Gecko browser extension")
      ("gitlab-bridge" "GitLab integration bridge")
      ("mozilla-extension" "Mozilla products extension")))

  (current-position
    (phase "consolidation")
    (status "initial-merge-complete")
    (notes "Five repos merged into monorepo structure")))
