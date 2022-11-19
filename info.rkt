#lang info

(define name "minikanren-ee")

(define deps '("base"
               "rackunit-lib"
               "threading"
               ["ee-lib" #:version "1.0"]))

(define test-omit-paths '("mk" "bench/common"))
(define compile-omit-paths '("bench/common"))

;(define build-deps '("racket-doc" "scribble-lib"))
;(define scribblings '(("main.scrbl" ())))
