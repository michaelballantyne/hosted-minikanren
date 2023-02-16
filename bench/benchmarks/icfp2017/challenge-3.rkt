#lang racket

(provide dynamic-then-lexical-3-expressions lexical-then-dynamic-3-expressions)

(require benchmark-minikanren)
(require "../stdlib/numbers.rkt")
(require "evalo-scoping.rkt")

(define (dynamic-then-lexical-3-expressions)
      (run 3 (expr)
           (eval-dyno expr 137)
           (eval-lexo expr 42)))


(define (lexical-then-dynamic-3-expressions)
      (run 3 (expr)
           (eval-lexo expr 42)
           (eval-dyno expr 137)))
