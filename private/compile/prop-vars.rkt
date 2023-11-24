#lang racket/base

(provide (all-defined-out))

;; attached by remove-unused-vars to goal-from-expression w/a list of all logic variables in scope at that expression
;;
;; properties keyed by this value will have type: [Listof Id]
(define TERM-VARS-IN-SCOPE 'term-vars-in-scope)

(define FIRST-REF 'first-ref)

(define SKIP-CHECK 'skip-occurs-check)
