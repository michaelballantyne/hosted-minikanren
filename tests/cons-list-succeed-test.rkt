#lang racket
(require "../main.rkt")


(module+ test
  (require (except-in rackunit fail))
  (test-equal?
    "cons and only works with cons"
   (run 1 (res2) (== (cons 1 res2) (cons 1 (cons 2 '()))))
   '((2)))

  (test-equal?
    "cons and quote play nicely together :("
   (run 1 (res2) (== (cons 1 res2) '(1 2)))
   '((2)))

  )
