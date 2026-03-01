; SPDX-License-Identifier: PMPL-1.0-or-later
; ECOSYSTEM.scm - Ecosystem placement for DYADT

(define ecosystem
  '((version . "1.0")
    (name . "did-you-actually-do-that")
    (type . "verification-framework")
    (purpose . "Verification framework that catches when LLMs claim work they haven't done, backed by cryptographic attestation chains, cross-modal code identity analysis, and a cladistic taxonomy of LLM failure modes")

    (position-in-ecosystem
      (role . "quality-gate")
      (domain . "ai-verification")
      (consumers . ("gitbot-fleet" "hypatia" "echidna" "panic-attacker")))

    (related-projects
      (verisimdb
        (relationship . "data-store")
        (description . "8-modal (octad) verification database — stores code identity across Document, Semantic, Graph, Temporal, Provenance, Vector, Tensor, and Spatial modalities. Cross-modal queries detect LLM fabrication patterns."))
      (panic-attacker
        (relationship . "attested-tool")
        (description . "Security vulnerability scanner with three-phase attestation chain (intent→evidence→seal). DYADT verifies panic-attack's attestation envelopes via 12 invariants, K9 cascade, and Trustfile."))
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
        (description . "Idris2 formally verified library — DYADT ABI builds on proven patterns"))
      (a2ml
        (relationship . "standard")
        (description . "Attested Markup Language — format for blueprints, envelopes, and contractiles"))
      (k9-svc
        (relationship . "standard")
        (description . "K9 Self-Validating Components — three-tier enforcement (Kennel/Yard/Hunt)"))
      (standards
        (relationship . "governance")
        (description . "Hyperpolymath standards monorepo — A2ML spec, K9 spec, contractile definitions")))

    (verification-philosophy
      (attestation-chain . "Three-phase: intent (before) → evidence (during) → seal (after)")
      (contractile-types . ("must" "trust" "dust" "lust" "k9" "anti"))
      (aop-mapping . "Contractile files = aspects, GUIDs = pointcuts, K9 = weaver")
      (hash-layers . "4 layers: document (L0), parts (L1), interactions (L2), phase (L3)")
      (meta-layer . "Guile Scheme — homoiconicity enables spec=validator=query=docs")
      (cladistic-taxonomy . "5 phyla: Structural, Semantic, Epistemic, Behavioral, Contextual")
      (code-identity . "VerisimDB octad — identity emerges from cross-modal coherence"))

    (dependencies
      (runtime . ("rust-nightly" "elixir-1.16" "otp-26"))
      (build . ("cargo" "mix" "zig-0.13" "idris2-0.7"))
      (meta . ("guile-3.0" "miniKanren"))
      (containers . ("chainguard-wolfi-base" "podman" "selur-compose" "cerro-torre" "vordr")))))
