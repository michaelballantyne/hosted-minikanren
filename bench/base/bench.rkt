#lang racket

(require "../../mk/mk.rkt")
(require "../../mk/numbers.rkt")
(require "../utils.rkt")
(require "four-fours.rkt")

(benchmark-suite "numbers"
  ["logo-hard" (run 9 (b q r) (logo (build-num 68) b q r) (>1o q))])

(benchmark-suite "four-fours"
  ["4" (four-fours 4)]
  ["256" (four-fours 256)])
