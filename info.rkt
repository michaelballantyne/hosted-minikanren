#lang info

(define name "minikanren-ee")

(define deps '("base"
               "rackunit-lib"
               "threading"
               "git://github.com/michaelballantyne/syntax-spec.git#main"))

(define build-deps '("db" "sql" "csv-reading" "text-table"))

(define test-omit-paths '("mk" "bench/benchmarks"))
(define compile-omit-paths '("bench/benchmarks"))
(define binary-omit-files '("bench" "demos"))

;(define build-deps '("racket-doc" "scribble-lib"))
;(define scribblings '(("main.scrbl" ())))
