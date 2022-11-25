#lang racket
(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")


(provide dynamic-then-lexical-3-expressions lexical-then-dynamic-3-expressions)

(include "../common/evalo-scoping.scm")

(define (dynamic-then-lexical-3-expressions)
      (run 3 (expr)
           (eval-dyno expr 137)
           (eval-lexo expr 42)))


(define (lexical-then-dynamic-3-expressions)
      (run 3 (expr)
           (eval-lexo expr 42)
           (eval-dyno expr 137)))
