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
  (pattern (x:id ...)
    #:fail-when (check-duplicate-identifier (syntax->list #'(x ...)))
    "duplicate variable name"))
(define-syntax-class bindings+/c
  #:description "binding list (<id> ...+)"
  (pattern (x:id ...+)
    #:fail-when (check-duplicate-identifier (syntax->list #'(x ...)))
    "duplicate variable name"))
(define-syntax-class define-header/c
  #:description "header (<name:id> <arg:id> ...)"
  (pattern (name:id v:id ...)
    #:fail-when (check-duplicate-identifier (syntax->list #'(v ...)))
    "duplicate parameter name"))
