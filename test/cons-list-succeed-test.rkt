#lang racket
(require "../main.rkt")



(module+ test
  (require rackunit)
  (test-equal?
   "confirms that cons and quote interplay nicely"
   (run 1 (res2) (== (cons 1 res2) '(1 2)))
   '((2))))
