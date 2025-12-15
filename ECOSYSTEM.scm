;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — patallm-gallery

(ecosystem
  (version "1.0.0")
  (name "patallm-gallery")
  (type "project")
  (purpose "*LLM tools gallery - unified verification and antidote systems*")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "*LLM tools gallery - unified verification and antidote systems*")
  (what-this-is-not "- NOT exempt from RSR compliance"))
