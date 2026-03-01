; SPDX-License-Identifier: PMPL-1.0-or-later
; META.scm - Meta-level information for claude-integrations

(meta
  (version "1.0")
  (license "PMPL-1.0-or-later")
  (author "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")

  (architecture-decisions
    (adr-001
      (title "Consolidate Claude integration repos into monorepo")
      (status "accepted")
      (date "2026-02-08")
      (context "Five separate repos with overlapping concerns and shared patterns")
      (decision "Merge into single monorepo with component subdirectories")
      (consequences "Simplified dependency management, shared tooling, single CI config")))

  (development-practices
    (build-tool "just")
    (license "PMPL-1.0-or-later")
    (documentation-format "AsciiDoc")))
