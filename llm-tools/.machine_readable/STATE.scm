;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; STATE.scm - Current project state for llm-tools
;;; Location: .machine_readable/STATE.scm ONLY

(state
  (metadata
    (version "1.0.0")
    (last-updated "2026-02-08")
    (updated-by "claude-opus-4.6"))

  (project-context
    (name "llm-tools")
    (type "monorepo")
    (description "Unified LLM tooling: context management, conversation unification, and code verification")
    (languages ("rust" "haskell" "guile-scheme")))

  (current-position
    (phase "consolidation")
    (status "initial-merge-complete")
    (completion-percentage 30))

  (components
    (antidote
      (status "imported")
      (origin "llm-antidote"))
    (unify
      (status "imported")
      (origin "llm-unify"))
    (unify-core
      (status "imported")
      (origin "llm-unify-core"))
    (verify
      (status "imported")
      (origin "llm-verify")))

  (critical-next-actions
    (action "Set up CI/CD workflows for monorepo")
    (action "Update internal cross-references between components")
    (action "Create GitHub repo and push")))
