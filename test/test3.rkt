#lang racket

(require "../main.rkt" racket/pretty)

(define-relation (conjo q)
  (fresh (x)
    (conde
      [(== x 5) (== q x)]
      [(== q 6)])))

(define-relation (disjo q)
  (conde
    [(fresh (x) (== q 6))]
    [(== q 7)]
    [(== q 8)]))

(module+ test
  (require rackunit)

  (check-equal?
    (run* (q) (conjo q))
    '(5 6))

  (check-equal?
    (run* (q) (disjo q))
    '(7 6 8)))
