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
  (list (cons "minikanren-ee" "../main.rkt")
        (cons "faster-minikanren" "base-lang.rkt")))


(module+ main
  (run-configurations "\n~a\n\n" 'main configurations))

(module+ test
  (run-configurations "testing benchmarks in ~a\n" 'test configurations))
