#lang racket

(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")
(require "../utils.rkt")

(benchmark-suite "numbers"
  ["logo-hard" (run 9 (b q r) (logo '(0 0 1 0 0 0 1) b q r) (>1o q))])
