; SPDX-License-Identifier: PMPL-1.0-or-later
; ECOSYSTEM.scm - Ecosystem placement for DYADT

(define ecosystem
  '((version . "1.0")
    (name . "did-you-actually-do-that")
    (type . "verification-framework")
    (purpose . "Catches when LLMs claim work they haven't done")

    (position-in-ecosystem
      (role . "quality-gate")
      (domain . "ai-verification")
      (consumers . ("gitbot-fleet" "hypatia" "echidna")))

    (related-projects
      (verisimdb
        (relationship . "data-store")
        (description . "6-modal verification database for storing scan results"))
      (panic-attacker
        (relationship . "sibling-tool")
        (description . "Security vulnerability scanner — complementary verification"))
      (hypatia
        (relationship . "consumer")
        (description . "Neurosymbolic CI/CD — consumes DYADT verification reports"))
      (gitbot-fleet
        (relationship . "consumer")
        (description . "Bot orchestration — finishbot uses DYADT for completion verification"))
      (cognitive-gating
        (relationship . "inspiration")
        (description . "Three-tier asymmetric model that DYADT implements"))
      (stapeln
        (relationship . "sibling-standard")
        (description . "Container ecosystem — DYADT follows Stacks pattern for context module"))
      (proven
        (relationship . "foundation")
        (description . "Idris2 formally verified library — DYADT ABI builds on proven patterns")))

    (dependencies
      (runtime . ("rust-nightly" "elixir-1.16" "otp-26"))
      (build . ("cargo" "mix" "zig-0.13" "idris2-0.7"))
      (containers . ("chainguard-wolfi-base" "podman" "selur-compose" "cerro-torre" "vordr")))))
