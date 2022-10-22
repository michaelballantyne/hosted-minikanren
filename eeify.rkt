#lang racket

(require threading)

(define (translate-rel sexp)
  (match sexp
    [`(define ,name (lambda ,args ,body))
     `(define-relation (,name ,@args) ,body)]
    [`(define (,name . ,args) ,body)
      `(define-relation (,name ,@args) ,body)]
    [_ sexp]))

(define (translate-module sexp)
  (match sexp
    [`(module ,name ,lang (#%module-begin . ,forms))
     `(module ,name ,lang (#%module-begin . ,(map translate-rel forms)))]))

(define (write-module sexp out-file)
  (parameterize ([current-output-port (open-output-file out-file #:exists 'replace)])
    (match sexp
      [`(module ,name ,lang (#%module-begin . ,forms))
       (displayln (~a "#lang " lang))
       (displayln "")
       (writeln '(require "main.rkt"))
       (displayln "")

       (for-each (λ (form) (pretty-display form) (newline)) forms)])))

(parameterize ([read-accept-reader #t]
               [read-accept-lang #t])
  (match-define (vector in out) (current-command-line-arguments))
  (~> (open-input-file in)
      read
      translate-module
      (write-module _ out)))
