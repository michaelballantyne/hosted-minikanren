#lang racket/base

(require "loader.rkt")

(displayln 'faster-mk)
(time
 (expand-with-redirections
  '(submod "benchmarks/bench.rkt" test)
  (hash 'benchmark-minikanren "langs/faster-mk.rkt")))

(displayln 'no-opts)
(time
 (expand-with-redirections
  '(submod "benchmarks/bench.rkt" test)
  (hash 'benchmark-minikanren "langs/ee-no-opt.rkt")))

(displayln 'opts)
(time
 (expand-with-redirections
  '(submod "benchmarks/bench.rkt" test)
  (hash 'benchmark-minikanren "langs/ee-all.rkt")))