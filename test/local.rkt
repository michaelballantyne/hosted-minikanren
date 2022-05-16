#lang racket

(require "../main.rkt"
         rackunit)

(define r
  (let ()
    (define-relation (foo x)
      (== x 5))

    (run 1 (q) (foo q))

    (relation (x) (foo x))))

(check-equal?
 (run 1 (q)
   (apply-relation r q))
 '(5))