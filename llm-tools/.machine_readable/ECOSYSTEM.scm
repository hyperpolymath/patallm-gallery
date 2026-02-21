;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; ECOSYSTEM.scm - Ecosystem position for llm-tools
;;; Location: .machine_readable/ECOSYSTEM.scm ONLY

(ecosystem
  (version "1.0.0")
  (name "llm-tools")
  (type "monorepo")
  (purpose "Unified LLM tooling suite for context management, conversation unification, and code verification")

  (position-in-ecosystem
    (domain "ai-tooling")
    (scope "llm-interaction-and-verification")
    (maturity "consolidation"))

  (related-projects
    (project "echidna"
      (relationship "dependency")
      (description "Code verification backend used by verify component"))
    (project "hyperpolymath/llm-antidote"
      (relationship "predecessor")
      (description "Original standalone repo, now antidote/ component"))
    (project "hyperpolymath/llm-unify"
      (relationship "predecessor")
      (description "Original standalone repo, now unify/ component"))
    (project "hyperpolymath/llm-unify-core"
      (relationship "predecessor")
      (description "Original standalone repo, now unify-core/ component"))
    (project "hyperpolymath/llm-verify"
      (relationship "predecessor")
      (description "Original standalone repo, now verify/ component"))))
