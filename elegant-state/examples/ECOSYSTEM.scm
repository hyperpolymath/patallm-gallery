;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; ECOSYSTEM.scm --- hyperpolymath project ecosystem
;;; Format: Guile Scheme S-expressions
;;; Updated: 2025-12-09

(define ecosystem
  '((metadata
     (name . "hyperpolymath")
     (version . "1.0")
     (updated . "2025-12-09"))

    (projects
     ;; === ACTIVE ===
     ((name . "conative-gating")
      (status . active)
      (phase . design)
      (description . "SLM-as-Cerebellum for LLM Policy Enforcement")
      (repository . "https://github.com/hyperpolymath/conative-gating")
      (languages rust elixir nickel))

     ((name . "elegant-STATE")
      (status . active)
      (phase . maintenance)
      (description . "Nickel schemas for policy and ecosystem")
      (repository . "https://github.com/hyperpolymath/elegant-STATE")
      (languages nickel racket))

     ((name . "NeuroPhone")
      (status . active)
      (phase . implementation)
      (description . "Neurosymbolic phone AI with reservoir computing")
      (languages rust elixir))

     ;; === ARCHIVED ===
     ((name . "echomesh")
      (status . archived)
      (archive-date . "2025-12-09")
      (superseded-by . "conative-gating")
      (description . "Context capture - superseded by prevention approach"))

     ((name . "UPM")
      (status . archived)
      (archive-date . "2025-12-09")
      (superseded-by . "conative-gating")
      (description . "Universal Project Manager - orchestration simplified"))

     ((name . "rhodibot")
      (status . archived)
      (archive-date . "2025-12-09")
      (description . "RSR enforcement bot - categories kept as SLM training material")
      (note . "categories/ preserved in conative-gating/training/")))

    (relationships
     ((from . "conative-gating") (to . "echomesh") (kind . supersedes))
     ((from . "conative-gating") (to . "UPM") (kind . supersedes))
     ((from . "conative-gating") (to . "rhodibot") (kind . supersedes))
     ((from . "elegant-STATE") (to . "conative-gating") (kind . part_of)))))
