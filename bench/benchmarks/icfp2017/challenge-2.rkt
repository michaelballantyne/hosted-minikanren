#lang racket

(provide four-thrines)

(require benchmark-minikanren)
(require "../stdlib/numbers.rkt")
(require "evalo-small.rkt")

(define (four-thrines)
  (run 4 (p q r)
       (=/= p q)
       (=/= p r)
       (=/= q r)
       (evalo p q)
       (evalo q r)
       (evalo r p)))
