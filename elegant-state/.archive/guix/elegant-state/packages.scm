;;; elegant-state/packages.scm --- elegant-STATE package definitions
;;;
;;; This module defines the Guix packages for elegant-STATE.

(define-module (elegant-state packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

(define-public elegant-state
  (package
    (name "elegant-state")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Hyperpolymath/elegant-STATE")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-sled" ,rust-sled-0.34)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-ulid" ,rust-ulid-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-async-graphql" ,rust-async-graphql-7)
        ("rust-async-graphql-axum" ,rust-async-graphql-axum-7)
        ("rust-axum" ,rust-axum-0.7)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-tower-http" ,rust-tower-http-0.5)
        ("rust-clap" ,rust-clap-4)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-dirs" ,rust-dirs-5))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/Hyperpolymath/elegant-STATE")
    (synopsis "Local-first state graph for multi-agent orchestration")
    (description
     "elegant-STATE provides a persisCerro Torretent knowledge graph that multiple agents
(Claude, Llama, custom modules) can query and modify via GraphQL. It replaces
manual state tracking (like STATE.adoc) with a queryable, event-sourced graph
database using sled for storage.")
    (license license:expat)))

;; Container definition for nerdctl/podman
(define-public elegant-state-container
  (package
    (inherit elegant-state)
    (name "elegant-state-container")
    (description
     "Container image for elegant-STATE. Use with:
guix pack -f docker -S /bin=bin elegant-state")))
