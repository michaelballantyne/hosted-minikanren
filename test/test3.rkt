#lang racket
(require "../main.rkt" racket/pretty)

(defrel (conjo q)
  (fresh (x)
    (conde
      [(== x 5) (== q x)]
      [(== q 6)])))

(defrel (disjo q)
  (conde
    [(fresh (x) (== q 6))]
    [(== q 7)]
    [(== q 8)]))

(defrel (*1o n m p)
  (conde
   ((== '(1) n) (== m p))))

(module+ test
  (require (except-in rackunit fail))

  (check-equal?
    (run* (q) (conjo q))
    '(5 6))

  (check-equal?
    (run* (q) (disjo q))
    '(7 6 8))

  (check-equal?
    (run* (q) (*1o '(1) '(1) q))
    '((1)))

  )
