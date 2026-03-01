;;; elegant-state/manifests.scm --- Development manifests for elegant-STATE
;;;
;;; Use with: guix shell -m guix/elegant-state/manifests.scm

(define-module (elegant-state manifests)
  #:use-module (guix profiles)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:export (development-manifest
            production-manifest))

;; Development manifest - includes build tools
(define development-manifest
  (packages->manifest
   (list
    ;; Rust toolchain
    rust
    rust-cargo

    ;; Build dependencies
    pkg-config
    openssl

    ;; Development tools
    git
    rust-analyzer)))

;; Production manifest - minimal runtime
(define production-manifest
  (packages->manifest
   (list openssl)))
