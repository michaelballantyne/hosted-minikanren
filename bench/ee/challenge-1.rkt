#lang racket
(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")


(provide first-love love-in-99000-ways)

(include "../common/evalo-standard.scm")

(define (first-love)
   (run 1 (q) (evalo q '(I love you))))

 (define (love-in-99000-ways)
    (run 99000 (q) (evalo q '(I love you))))

(module+ test
  (require rackunit)
  (test-equal?
   "first-love"
   (first-love)
   '('(I love you)))
)
