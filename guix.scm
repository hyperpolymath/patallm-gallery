; SPDX-License-Identifier: PMPL-1.0-or-later
;; guix.scm — GNU Guix package definition for patallm-gallery
;; Usage: guix shell -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses))

(package
  (name "patallm-gallery")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (synopsis "patallm-gallery")
  (description "patallm-gallery — part of the hyperpolymath ecosystem.")
  (home-page "https://github.com/hyperpolymath/patallm-gallery")
  (license ((@@ (guix licenses) license) "PMPL-1.0-or-later"
             "https://github.com/hyperpolymath/palimpsest-license")))
