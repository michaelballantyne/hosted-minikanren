#lang racket/base
(require "../main.rkt")

;; TODO

;; This should be turned into an error at the expander layer instead
;; of errors in later passes.
;;
;; We have decided to enforce a syntactic restriction that first class
;; relations may not have free logic variable references.
;;
;; This potentially makes the feature less expressive, and maybe some
;; day we’ll go in and add back that expressivity, but for now this
;; makes the compiler passes easier because under this restriction,
;; the pass gets to see the binding of every logic variable.
;;

#;(define-relation (bar y)
  (apply-relation
   (relation (x)
     (== x y))
   y))

#;(run 1 (q) (bar2 q))

#;(run 1 (q)
     (apply-relation
      (relation (x)
        (== x q))
      q))
