#lang racket

(require syntax/parse)
(require (for-template "forms.rkt"))

(provide goal/c
         term/c
         bindings/c
         bindings+/c
         define-header/c
         
         unary-constraint
         binary-constraint
         binary-goal-constructor)

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

(define-syntax-class unary-constraint
  #:literal-sets (mk-literals)
  (pattern (~or symbolo stringo numbero)))
(define-syntax-class binary-constraint
  #:literal-sets (mk-literals)
  (pattern (~or == =/= absento)))
(define-syntax-class binary-goal-constructor
  #:literal-sets (mk-literals)
  (pattern (~or conj disj)))


