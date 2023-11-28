#lang racket

(provide first-love love-in-9900-ways)

(require benchmark-minikanren)
(require "../stdlib/numbers.rkt")
(require "evalo-standard.rkt")

(define (first-love)
   (run 1 (q) (evalo q '(I love you))))

 (define (love-in-9900-ways)
    (run 9900 (q) (evalo q '(I love you))))

(module+ test
  (require (except-in rackunit fail))
  (test-equal?
   "first-love"
   (first-love)
   '('(I love you)))
)
