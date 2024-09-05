#lang info

(define name "hosted-minikanren")

(define deps '("base"
               "rackunit-lib"
               "threading"
               "git://github.com/michaelballantyne/syntax-spec.git#main"))

(define build-deps '("db" "sql" "csv-reading" "text-table" "fmt"))

(define test-omit-paths '("mk" "bench/benchmarks"))
(define compile-omit-paths '("bench/benchmarks"))
(define binary-omit-files '("bench" "demos"))

(define license 'MIT)

(define scribblings '(("scribblings/main.scrbl" () (experimental) "hosted-minikanren")))

;(define build-deps '("racket-doc" "scribble-lib"))
;(define scribblings '(("main.scrbl" ())))
