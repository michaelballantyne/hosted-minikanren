#lang racket

(require rackunit
         syntax/macro-testing
         (for-syntax syntax/parse "unit-test-infra.rkt"))

(provide (for-syntax generate-prog) progs=?)

(define-syntax (progs=? stx)
  (syntax-parse stx
    [(_ p1 p2) #'(check-true (phase1-eval (alpha=? p1 p2)))]))

