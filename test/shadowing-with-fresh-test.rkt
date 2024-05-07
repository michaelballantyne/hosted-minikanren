#lang racket/base

(require (except-in rackunit fail)
         "../main.rkt")

;; Regresssion tests for problems caused by shadowing, in case the identifier
;; tables used in various compiler passes fail to distinguish bindings of the same name.

;; This caused a compile-time infinite loop in constant folding.
(defrel (uh-o ls)
  (fresh (m)
    (== ls `(,m)) ;; substitution maps ls -> (list m)
    (fresh (ls)
      (== ls m)   ;; ls bound, m unbound, so bind m -> ls
      (uh-o m)))) ;; so now inlining chases m -> ls -> (list m) -> (list ls) -> (list (list m)) -> ...

;; Previously constant folding conflated the two `x`s, replacing the first macro-generated
;; unify with `succeed` and producing a result of 1 for this query.
(define-syntax m
  (goal-macro
    (syntax-rules ()
      [(_ a b)
       (fresh (x)
         (== a x)
         (== b x))])))
(check-equal?
 (run 1 (x) (m x 2) (== x 1))
 '())

;; Previously constant folding conflated the two `x`s, resulting in a spurious occurs-check failure.
(check-equal?
 (run 1 (x) (m (list x) (list x)))
 '(_.0))

;; This caused a runtime infinite loop due to a cyclic term, created because of an
;; improperly skipped occurs check.
(defrel (r y)
  (fresh (x y z)
    (conde ;; ensure the cycle isn't caught by constant folding...
      [(== x (list y))]
      [fail])
    (fresh (x)
      (== x 2)) ;; trick the occurs check pass into thinking x holds a constant
    (== y x) ;; skipping occurs check here creates a cycle
    (r2 x))) ;; pass term with cycle into r2 to diverge in unification
(defrel (r2 c)
  (fresh (z)
    ;; use term-from-expression to force an unspecialized unify that will diverge given a cycle
    (== c (term-from-expression (list z)))))
(check-equal?
 (run* (q) (r q))
 '())

;; Check that the occurs check analysis doesn't conflate names and improperly skip an occurs check.
;; Constant folding gives up on a branch, but the occurs check analysis doesn't, so we can use a branch
;; to sneak an occurs violation pass constant folding.
(define-syntax m2
  (goal-macro
    (syntax-rules ()
      [(_ a b)
       (fresh (x)
         (conde
           [(== a x)]
           [fail])
         (== b x))])))
(check-equal?
 (run 1 (x)
   (m2 (list x) x))
 '())