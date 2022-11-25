#lang racket
(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")

(provide four-thrines)

(include "../common/evalo-small.scm")

(define (four-thrines)
  (run 4 (p q r)
       (=/= p q)
       (=/= p r)
       (=/= q r)
       (evalo p q)
       (evalo q r)
       (evalo r p)))
