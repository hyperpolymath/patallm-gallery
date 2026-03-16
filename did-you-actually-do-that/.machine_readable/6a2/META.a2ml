; SPDX-License-Identifier: PMPL-1.0-or-later
; META.scm - Architecture decisions and design rationale for DYADT

(define meta
  '((metadata
      (version . "1.0")
      (project . "did-you-actually-do-that")
      (updated . "2026-02-12"))

    (architecture-decisions
      (adr-001
        (title . "12-Layer Verification Pipeline")
        (status . "accepted")
        (context . "Need comprehensive verification of AI-claimed work")
        (decision . "Implement 12 distinct verification layers from file existence to pattern learning")
        (consequences . "High accuracy but requires both Rust and Elixir components"))

      (adr-002
        (title . "Rust + Elixir Dual Runtime")
        (status . "accepted")
        (context . "Need fast verification (Rust) and supervision/API (Elixir)")
        (decision . "Rust engine for layers 1-9 and 11, Elixir brain for layers 5/10/12 and API")
        (consequences . "Erlang Port Protocol bridges the two runtimes"))

      (adr-003
        (title . "Asymmetric Vote Weighting")
        (status . "accepted")
        (context . "False positives (missing bad work) are worse than false negatives")
        (decision . "Rejection votes get 1.5x weight, approvals get 1.0x")
        (consequences . "System is naturally conservative — biased toward catching problems"))

      (adr-004
        (title . "Graph-of-Thought + Mixture-of-Experts")
        (status . "accepted")
        (context . "Single evaluator can be fooled; need adversarial diversity")
        (decision . "GoT uses 3 adversarial paths; MoE routes to specialized experts")
        (consequences . "More thorough evaluation but more computation per claim"))

      (adr-005
        (title . "Triple API Surface")
        (status . "accepted")
        (context . "Different consumers need different protocols")
        (decision . "REST + GraphQL on port 4200, gRPC on port 4201")
        (consequences . "All three call through single Verifications context module"))

      (adr-006
        (title . "Idris2 ABI + Zig FFI")
        (status . "accepted")
        (context . "Need formally verified interface for cross-language use")
        (decision . "Idris2 for type proofs, Zig for C-compatible implementation")
        (consequences . "Any language can call DYADT through standard C ABI")))

    (development-practices
      (testing . "204 tests (130 Rust + 74 Elixir)")
      (license . "PMPL-1.0-or-later")
      (containers . "Chainguard wolfi-base, Podman, selur-compose")
      (ci-cd . "Planned: GitHub Actions with hypatia-scan"))

    (design-rationale
      (cognitive-gating . "Three-tier model from cognitive science applied to verification")
      (pessimistic-union . "GoT aggregation: if ANY path finds a problem, it's reported")
      (violation-routing . "MoE only activates relevant experts per claim for efficiency")
      (pattern-learning . "Track violation frequency to predict future problems"))))
