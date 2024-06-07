#lang info

(define name "minikanren-ee")

(define deps '("base"
               "rackunit-lib"
               "threading"
               "syntax-spec"))

(define build-deps '("db" "sql"))

(define test-omit-paths '("mk" "bench/benchmarks" "bench/process-bench.rkt"))
(define compile-omit-paths '("bench/benchmarks" "bench/process-bench.rkt"))

;(define build-deps '("racket-doc" "scribble-lib"))
;(define scribblings '(("main.scrbl" ())))
