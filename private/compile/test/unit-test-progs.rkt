#lang racket

(require syntax/macro-testing
         rackunit
         (for-syntax syntax/parse "unit-test-infra.rkt"))

(provide (for-syntax generate-prog) progs=?)

(define-syntax (progs=? stx)
  (syntax-parse stx
    [(_ p1 p2)
     #'(let ([res-info (phase1-eval (get-test-result (alpha=? p1 p2)))])
         (check-true (car res-info) (cdr res-info)))]))
     ;; #'(phase1-eval (get-test-result (alpha=? p1 p2)))]))
         ;; (let ([res (alpha=? p1 p2)])
         ;;   (get-test-result res)))]))
     ;; #'(let ([foobar (phase1-eval (get-test-result (alpha=? p1 p2)))])
     ;;     (check-true foobar foobar))]))
