#lang racket/base

(require "loader.rkt")

(define (run-configurations message submod configurations)
  (for ([configuration configurations])
    (define lang-name (car configuration))
    (define lang-path (cdr configuration))

    (printf message lang-name)
    (run-with-redirections `(submod "benchmarks/bench.rkt" ,submod)
                           (hash 'benchmark-minikanren lang-path))))

(define configurations
  (list
   (cons "faster-minikanren" "langs/faster-mk.rkt")
   (cons "no optimizations" "langs/ee-no-opt.rkt")
   (cons "prop-only" "langs/ee-prop-only.rkt")
   (cons "dead-code" "langs/ee-dead-code.rkt")
   (cons "occurs-check" "langs/ee-occurs-check.rkt")
   (cons "all optimizations" "langs/ee-all.rkt")))


(module+ main
  (run-configurations "\n~a\n\n" 'main configurations))

(module+ test
  (run-configurations "testing benchmarks in ~a\n" 'test configurations))
