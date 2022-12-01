#lang racket
(require "../../mk/mk.rkt")

(provide first-love love-in-9900-ways)

(include "../common/evalo-standard.scm")

(define (first-love)
   (run 1 (q) (evalo q '(I love you))))

 (define (love-in-9900-ways)
    (run 9900 (q) (evalo q '(I love you))))

(module+ test
  (require rackunit)
  (test-equal?
   "first-love"
   (first-love)
   '('(I love you)))
)
