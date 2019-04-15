#lang racket

(require minikanren-ee)

(require (submod minikanren-ee/core private)
         (for-syntax syntax/parse syntax-generic2)
         syntax-generic2/define)

(define-syntax/generics (rkt-term2 e)
  [(core-term)
   this-syntax]
  [(compile)
   (syntax/loc #'e (invariant-assertion mk-value? e))]
  [(map-transform f) (f this-syntax)])

(define-relation (appendo l1 l2 l3)
  (conde
   [(== l1 '()) (== l3 l2)]  ; base case
   [(fresh (head rest) ; recursive case
      (== `(,head . ,rest) l1)
      (fresh (result)
        (appendo rest l2 result)
        (== `(,head . ,result) l3)))]))

(module+ test
  (require
    rackunit
    (only-in
      racket/base
      [quote racket-quote]
      [cons racket-cons]))

  (check-equal?
    (run 2 (q) (appendo `(1 2 3) `(4 5) q))
    (racket-quote ((1 2 3 4 5))))
  (check-equal?
    (run 1 (q) (== (rkt-term (make-list 5 "a")) q))
    (racket-quote (("a" "a" "a" "a" "a"))))
  (check-equal?
    (run 1 (q) (== (rkt-term2 (make-list 5 "a")) q))
    (racket-quote (("a" "a" "a" "a" "a"))))

  )
