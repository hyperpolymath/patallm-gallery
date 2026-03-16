;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; META.scm - Meta-level information for llm-tools
;;; Location: .machine_readable/META.scm ONLY

(meta
  (version "1.0.0")
  (last-updated "2026-02-08")

  (architecture-decisions
    (adr "001"
      (title "Consolidate LLM repos into monorepo")
      (status "accepted")
      (date "2026-02-08")
      (rationale "Four tightly-related LLM projects benefit from shared build infrastructure, unified CI/CD, and reduced maintenance overhead")
      (components "llm-antidote" "llm-unify" "llm-unify-core" "llm-verify")))

  (development-practices
    (build-system "just")
    (rust-edition "2021")
    (rust-minimum "1.75")
    (haskell-resolver "cabal")
    (license "PMPL-1.0-or-later"))

  (design-rationale
    (monorepo-structure "Each component retains its own build files and can be developed independently, while sharing root-level orchestration and CI/CD")
    (language-diversity "Rust for performance-critical conversation management, Haskell for verification with strong type guarantees, Guile Scheme for symbolic reset artifacts")))
