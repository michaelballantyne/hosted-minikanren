#lang racket

(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class goal/c
  #:description "goal expression"
  (pattern _))
(define-syntax-class term/c
  #:description "term expression"
  (pattern _))
(define-syntax-class bindings/c
  #:description "binding list (<id> ...)"
  (pattern (x:id ...)))
(define-syntax-class bindings+/c
  #:description "binding list (<id> ...+)"
  (pattern (x:id ...+)))
(define-syntax-class define-header/c
  #:description "header (<name:id> <arg:id> ...)"
  (pattern (name:id v:id ...)))