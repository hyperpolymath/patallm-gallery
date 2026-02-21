; SPDX-License-Identifier: PMPL-1.0-or-later
; STATE.scm - Current project state for did-you-actually-do-that (DYADT)

(define state
  '((metadata
      (version . "0.1.0")
      (schema-version . "1.0")
      (created . "2026-01-17")
      (updated . "2026-02-12")
      (project . "did-you-actually-do-that")
      (repo . "https://github.com/hyperpolymath/did-you-actually-do-that"))

    (project-context
      (name . "Did You Actually Do That?")
      (tagline . "Verification framework that catches when LLMs claim work they haven't done")
      (tech-stack . (rust elixir idris2 zig absinthe plug bandit)))

    (current-position
      (phase . "v0.1.0-complete")
      (overall-completion . 95)
      (components
        (verification-pipeline . 100)
        (slm-ensemble . 100)
        (triple-api . 100)
        (abi-ffi . 100)
        (pattern-learning . 100)
        (container-deployment . 100)
        (real-slm-inference . 0)
        (tests . 90)
        (ci-cd . 0)
        (documentation . 95))
      (working-features
        "12-layer verification pipeline (all layers)"
        "SLM ensemble with GoT + MoE backends"
        "Triple API: REST + GraphQL + gRPC"
        "Claim extraction from LLM text"
        "Policy Oracle with 12 violation categories"
        "Asymmetric voting (1.5x for rejections)"
        "Pattern learning with ETS-backed frequency analysis"
        "Regression guard with baseline comparison"
        "Cross-reference graph analysis"
        "Completeness audit (stubs, orphans, gaps)"
        "Erlang Port Protocol (Rust <-> Elixir)"
        "Idris2 ABI with formal proofs"
        "Zig FFI C-compatible implementation"
        "Container deployment via selur-compose"
        "204 tests (130 Rust + 74 Elixir), 0 failures"))

    (route-to-mvp
      (milestone-1
        (name . "Core Complete")
        (status . "done")
        (items
          "Define Claim, Evidence, Verdict types"
          "Implement Verifier with standard checkers"
          "CLI tool with basic commands"))
      (milestone-2
        (name . "Full Pipeline")
        (status . "done")
        (items
          "All 12 verification layers implemented"
          "SLM ensemble with GoT + MoE"
          "Triple API (REST + GraphQL + gRPC)"
          "Idris2 ABI + Zig FFI"
          "Container deployment"))
      (milestone-3
        (name . "Production Ready")
        (status . "planned")
        (items
          "Real SLM inference (llama.cpp)"
          "CI/CD workflows"
          "Publish to crates.io"
          "gitbot-fleet integration")))

    (blockers-and-issues
      (critical . ())
      (high . ())
      (medium
        ("Real SLM inference not yet integrated (llama.cpp)"
         "No CI/CD workflows"))
      (low
        ("Codeberg mirror not yet set up")))

    (critical-next-actions
      (immediate
        "Push to GitHub and GitLab")
      (this-week
        "Add CI/CD workflows"
        "Integrate llama.cpp for real SLM inference")
      (this-month
        "Publish to crates.io"
        "Integrate with gitbot-fleet"))

    (session-history
      ((date . "2026-01-17")
       (accomplishments
         "Initial project creation"
         "Core library implementation"
         "CLI tool implementation"))
      ((date . "2026-02-12")
       (accomplishments
         "Phase 1: Completed all 12 verification layers"
         "Phase 2: Triple API (REST + GraphQL + gRPC)"
         "Phase 3: SLM ensemble with GoT + MoE"
         "Phase 4: Idris2 ABI + Zig FFI customization"
         "Phase 5: Documentation, config, push"
         "204 tests total, 0 failures")))))

; Helper functions
(define (get-completion-percentage state)
  (cdr (assoc 'overall-completion (cdr (assoc 'current-position state)))))

(define (get-blockers state priority)
  (cdr (assoc priority (cdr (assoc 'blockers-and-issues state)))))

(define (get-milestone state name)
  (let ((milestones (cdr (assoc 'route-to-mvp state))))
    (find (lambda (m) (equal? (cdr (assoc 'name (cdr m))) name)) milestones)))
