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

(run 1 (q) (== q 'cat))

(run 1 (q) (absento 'cat q))

(test-goal-syntax (fresh (x y) (== y x)))
(test-goal-syntax (fresh (y) (absento (list 'cat 'cat 'cat) y)))
