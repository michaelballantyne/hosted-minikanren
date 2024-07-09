#lang racket
(require (except-in rackunit fail))
(require "./minikanren-spec-compiler.rkt")

(run 1 (x) (== x x))
(run 1 (x) (== x (quote cat)))

(defrel (foo x y z)
  (== x y))

;; Buggy
#;(run 'fish (x) succeed)
#;(run 1 (q) (== (list x x) q))
