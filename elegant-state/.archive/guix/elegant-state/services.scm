;;; elegant-state/services.scm --- Guix system services for elegant-STATE
;;;
;;; This module provides Shepherd services for running elegant-STATE
;;; as a system service.

(define-module (elegant-state services)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (elegant-state packages)
  #:export (elegant-state-service-type
            elegant-state-configuration
            elegant-state-configuration?))

(define-record-type* <elegant-state-configuration>
  elegant-state-configuration make-elegant-state-configuration
  elegant-state-configuration?
  (package     elegant-state-configuration-package
               (default elegant-state))
  (host        elegant-state-configuration-host
               (default "127.0.0.1"))
  (port        elegant-state-configuration-port
               (default 4000))
  (data-dir    elegant-state-configuration-data-dir
               (default "/var/lib/elegant-state"))
  (user        elegant-state-configuration-user
               (default "elegant-state"))
  (group       elegant-state-configuration-group
               (default "elegant-state")))

(define (elegant-state-shepherd-service config)
  "Return a Shepherd service for elegant-STATE."
  (let ((package  (elegant-state-configuration-package config))
        (host     (elegant-state-configuration-host config))
        (port     (elegant-state-configuration-port config))
        (data-dir (elegant-state-configuration-data-dir config))
        (user     (elegant-state-configuration-user config))
        (group    (elegant-state-configuration-group config)))
    (list (shepherd-service
           (documentation "Run elegant-STATE GraphQL server")
           (provision '(elegant-state))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/state-cli")
                           "serve" "http"
                           "--host" #$host
                           "--port" (number->string #$port)
                           "--db-path" (string-append #$data-dir "/db"))
                     #:user #$user
                     #:group #$group
                     #:directory #$data-dir))
           (stop #~(make-kill-destructor))))))

(define elegant-state-accounts
  (lambda (config)
    (let ((user (elegant-state-configuration-user config))
          (group (elegant-state-configuration-group config)))
      (list (user-group (name group) (system? #t))
            (user-account
             (name user)
             (group group)
             (system? #t)
             (home-directory "/var/empty")
             (shell "/run/current-system/profile/bin/nologin"))))))

(define elegant-state-service-type
  (service-type
   (name 'elegant-state)
   (description "Run the elegant-STATE GraphQL server")
   (extensions
    (list (service-extension shepherd-root-service-type
                            elegant-state-shepherd-service)
          (service-extension account-service-type
                            elegant-state-accounts)))
   (default-value (elegant-state-configuration))))
