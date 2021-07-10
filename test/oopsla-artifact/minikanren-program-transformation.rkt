#lang racket/base

(require minikanren-ee racket/pretty)

(defrel (append l1 l2 l3)
  (disj
    (conj (== l1 '()) (== l2 l3))
    (fresh (first rest result)
      (conj (== (cons first rest) l1)
            (append rest l2 result)
            (== (cons first result) l3)))))

(pretty-print (syntax->datum (relation-code append)))

(module+ test
  (require rackunit)

  (check-equal?
    (run 4 (l1 l2) (append l1 l2 '(1 2)))
    '((() (1 2)) ((1) (2)) ((1 2) ()))))

