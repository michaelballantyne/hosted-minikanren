#lang racket

(require "../../main.rkt" racket/pretty)

(defrel/match (append l1 l2 l3)
  [(() ,rest ,rest)]
  [((,first . ,rest) ,?? (,first . ,result))  (append rest l2 result)])

(pretty-print (syntax->datum (relation-code append)))

(module+ test
  (require (except-in rackunit fail))

  (check-equal?
    (run 1 (q) (append '() '() q))
    '(()))

  (check-equal?
    (run 3 (l1 l2) (append l1 l2 '(1 2)))
    '((() (1 2)) ((1) (2)) ((1 2) ()))))
