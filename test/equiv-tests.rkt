#lang racket
(require "../main.rkt")

(module+ test
  (require rackunit)

  ;; A complicated relation to produce 2 as a value.
  (defrel (is-two? l1)
    (fresh (doozle boozle)
           (conj (== doozle (cons 7 boozle))
                 (== boozle `(2 . 5))
                 (== doozle (cons 7 (cons l1 5))))))

  (test-equal?
   "We successfully codegen the walks for variable subterms in complex t2's"
   (run 1 (l1) (is-two? l1))
   '(2))

  )
