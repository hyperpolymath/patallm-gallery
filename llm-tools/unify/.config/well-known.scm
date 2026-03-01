;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Well-Known Configuration for LLM Unify
;; This file defines the .well-known resources for RFC compliance

(define-module (llm-unify well-known)
  #:export (security-txt ai-txt humans-txt))

;; RFC 9116 Security Contact Information
(define security-txt
  '((contact . "mailto:security@hyperpolymath.dev")
    (expires . "2026-12-31T23:59:59Z")
    (preferred-languages . "en")
    (canonical . "https://github.com/hyperpolymath/llm-unify/.well-known/security.txt")
    (policy . "https://github.com/hyperpolymath/llm-unify/blob/main/SECURITY.md")
    (acknowledgments . "https://github.com/hyperpolymath/llm-unify/blob/main/SECURITY.md#acknowledgments")))

;; AI Policy Declaration
(define ai-txt
  '((name . "LLM Unify")
    (description . "Local-first LLM conversation management tool")
    (ai-usage . "This project uses AI-assisted development")
    (training-opt-out . #t)
    (scraping-policy . "robots.txt")
    (data-retention . "local-only")))

;; Human Contributors
(define humans-txt
  '((team . "LLM Unify Contributors")
    (site . "https://github.com/hyperpolymath/llm-unify")
    (last-updated . "2025-12-27")
    (language . "Rust")
    (standards . ("RFC 9116" "AGPL-3.0" "RSR Silver"))))
