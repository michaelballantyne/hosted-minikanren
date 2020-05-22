#lang racket

(require "../main.rkt")

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
    rackunit)

  (check-equal?
    (run 2 (q) (appendo `(1 2 3) `(4 5) q))
    '((1 2 3 4 5)))
  (check-equal?
    (run 1 (q) (== (rkt-term (make-list 5 "a")) q))
    '(("a" "a" "a" "a" "a")))

  )

(relation-code appendo)
