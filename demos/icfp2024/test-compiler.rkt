#lang racket
(require (except-in rackunit fail))
(require "./minikanren-spec-compiler.rkt")

(run 1 (x) (== x x))

;; Buggy
#;(run 'fish (x) succeed)
#;(run 1 (q) (== (list x x) q))
