#lang racket/base

(require "../../main.rkt")

(defrel (append l1 l2 l3)
  (disj
    (conj (== l1 '()) (== l2 l3))
    (fresh (first rest result)
      (conj (== (cons first rest) l1)
            (== (cons first result) l3)
            (append rest l2 result)))))

(module+ test
  (require (except-in rackunit fail))

  (check-equal?
    (run 3 (l1 l2) (append l1 l2 '(1 2)))
    '((() (1 2)) ((1) (2)) ((1 2) ()))))

