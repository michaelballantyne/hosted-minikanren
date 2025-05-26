#lang racket

(require (except-in rackunit fail)
         "../main.rkt")

;; This example tests the FIRST-REF case of generate-matching-unify-body,
;; because q1 is fresh, is not bound in the block that unifies it, and
;; is not removable as dead code. A previous implementation missed handling
;; this case.

(defrel (r x) (fresh (q1) (fresh () (== x (list q1)))))
(check-equal?
 (run 1 (q) (r q))
 '((_.0)))