#lang racket/load

(require "../main.rkt" rackunit)

(defrel (ones x)
  (conde
    [(== x 1)]
    [(ones x)]))

(check-equal?
  (run 2 (q) (ones q))
  '(1 1))
