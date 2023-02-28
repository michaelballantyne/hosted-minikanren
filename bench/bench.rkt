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
    (cons "minikanren-ee no optimizations" "langs/ee-no-opt.rkt")
    (cons "minikanren-ee all optimizations" "langs/ee-all.rkt")
        
        (cons "minikanren-ee prop-only" "langs/ee-prop-only.rkt")
        (cons "minikanren-ee dead-code" "langs/ee-dead-code.rkt")
        (cons "minikanren-ee occurs-check" "langs/ee-occurs-check.rkt")
        (cons "faster-minikanren" "langs/faster-mk.rkt")))


(module+ main
  (run-configurations "\n~a\n\n" 'main configurations))

(module+ test
  (run-configurations "testing benchmarks in ~a\n" 'test configurations))
