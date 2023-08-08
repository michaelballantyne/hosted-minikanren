#lang info

(define name "minikanren-ee")

(define deps '("base"
               "rackunit-lib"
               "threading"
               ["ee-lib" #:version "1.0"]
               "syntax-spec"))

(define test-omit-paths '("mk" "bench/benchmarks" "bench/process-bench.rkt"))
(define compile-omit-paths '("bench/benchmarks" "bench/process-bench.rkt"))

;(define build-deps '("racket-doc" "scribble-lib"))
;(define scribblings '(("main.scrbl" ())))
