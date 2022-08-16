#lang racket

(require "../../mk/mk.rkt")
(require "../../mk/numbers.rkt")
(require "../utils.rkt")
(require "four-fours.rkt")

(benchmark-suite "numbers"
  ["logo-hard" (run 9 (b q r) (logo (build-num 68) b q r) (>1o q))])

(benchmark-suite "four-fours"
  ["4" (run 1 (q) (four-fours (build-num 4)))]
  ["256" (run 1 (q) (four-fours 256))])

