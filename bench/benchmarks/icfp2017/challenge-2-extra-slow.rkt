#lang racket

(provide twine-slow thrine-super-duper-slow)

(require benchmark-minikanren)
(require "../stdlib/numbers.rkt")
(require "evalo-standard.rkt")

(define (twine-slow)
  (run 1 (p q)
       (=/= p q)
       (evalo p q)
       (evalo q p)))

(define (thrine-super-duper-slow)
  (run 1 (p q r)
       (=/= p q)
       (=/= p r)
       (=/= q r)
       (evalo p q)
       (evalo q r)
       (evalo r p)))
