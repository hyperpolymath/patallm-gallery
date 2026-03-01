#lang racket/base

;;; to-scm.rkt --- Export Nickel ecosystem config to ECOSYSTEM.scm
;;;
;;; Usage: racket to-scm.rkt <input.ncl> > ECOSYSTEM.scm
;;;
;;; This script reads a Nickel ecosystem configuration and outputs
;;; a Scheme S-expression format suitable for homoiconic state persistence.

(require json
         racket/port
         racket/string
         racket/list
         racket/format)

;; Read JSON from nickel export
(define (read-nickel-json path)
  (define nickel-output
    (with-output-to-string
      (lambda ()
        (system* (find-executable-path "nickel")
                 "export"
                 "--format" "json"
                 path))))
  (string->jsexpr nickel-output))

;; Convert JSON to S-expression format
(define (json->sexp j)
  (cond
    [(hash? j)
     (cons 'record
           (for/list ([(k v) (in-hash j)])
             (cons (string->symbol k) (json->sexp v))))]
    [(list? j)
     (cons 'list (map json->sexp j))]
    [(string? j) j]
    [(number? j) j]
    [(boolean? j) (if j '#t '#f)]
    [(eq? j 'null) '()]
    [else j]))

;; Pretty-print S-expression as Scheme
(define (sexp->scheme s [indent 0])
  (define pad (make-string indent #\space))
  (cond
    [(and (pair? s) (eq? (car s) 'record))
     (string-append
      "(\n"
      (string-join
       (for/list ([pair (cdr s)])
         (format "~a  (~a . ~a)"
                 pad
                 (car pair)
                 (sexp->scheme (cdr pair) (+ indent 4))))
       "\n")
      (format "\n~a)" pad))]
    [(and (pair? s) (eq? (car s) 'list))
     (format "(~a)" (string-join (map ~a (cdr s)) " "))]
    [(string? s) (format "~s" s)]
    [(symbol? s) (format "'~a" s)]
    [else (~a s)]))

;; Main entry point
(define (main args)
  (when (null? args)
    (displayln "Usage: racket to-scm.rkt <input.ncl>" (current-error-port))
    (exit 1))

  (define input-path (car args))
  (define json-data (read-nickel-json input-path))
  (define sexp-data (json->sexp json-data))

  (displayln ";;; ECOSYSTEM.scm --- Generated from Nickel config")
  (displayln (format ";;; Source: ~a" input-path))
  (displayln (format ";;; Generated: ~a" (date->string (current-date) #t)))
  (displayln "")
  (displayln "(define ecosystem")
  (displayln (format "  ~a)" (sexp->scheme sexp-data 2))))

(module+ main
  (main (vector->list (current-command-line-arguments))))
