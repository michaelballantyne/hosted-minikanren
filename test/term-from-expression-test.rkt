#lang racket

(require "../main.rkt"
         (except-in rackunit fail))

(define jugs02 5)

(defrel (water-jug-sequence seq)
  (fresh (st)
    (== seq (cons (term-from-expression jugs02) 6))))

(check-equal?
 (run 1 (q)
   (water-jug-sequence q))
 '(???))
