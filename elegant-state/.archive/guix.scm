;;; guix.scm --- Development shell for elegant-STATE
;;;
;;; Use with: guix shell
;;; Or: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix build-system cargo)
             (gnu packages rust)
             (gnu packages rust-apps)
             (gnu packages pkg-config)
             (gnu packages tls)
             (gnu packages version-control))

;; Development dependencies for building elegant-STATE
(packages->manifest
 (list
  ;; Rust toolchain
  rust
  rust-cargo

  ;; Build dependencies
  pkg-config
  openssl

  ;; Development tools
  git))
