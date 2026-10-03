#lang info

(define name "hosted-minikanren")

(define deps '("csv-reading"
               "db-lib"
               "fmt"
               "sql"
               "text-table"
               "base"
               "rackunit-lib"
               "threading"
               "syntax-spec-v2"))

(define build-deps '("racket-doc"
                     "scribble-lib"
                     "db" "sql" "csv-reading" "text-table" "fmt"))

(define test-omit-paths '("mk" "bench/benchmarks"))
(define compile-omit-paths '("bench/benchmarks"))
(define binary-omit-files '("bench" "demos"))

(define license 'MIT)

(define scribblings '(("scribblings/main.scrbl" () (experimental) "hosted-minikanren")))

;(define build-deps '("racket-doc" "scribble-lib"))
;(define scribblings '(("main.scrbl" ())))
