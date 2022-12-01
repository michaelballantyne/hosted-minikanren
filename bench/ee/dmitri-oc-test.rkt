#lang racket

(require "../../main.rkt")
(include "../common/dmitri-peano-suite.scm")

(provide leo pluso multo1 multo2 lengthdo lengtho incr-listo appendo reverseo1 reverseo2 leo-801)

(module+ test
  (require rackunit)

  (check-equal?
   (run 1 (q) (leo '(S S S . 0) '(S S . 0)))
   '())

  (check-equal?
   (run 1 (q) (leo '(S S . 0) '(S S S . 0)))
   '(_.0))

  (check-equal?
   (run 1 (q) (pluso '(S S . 0) '(S S S . 0) q))
   '((S S S S S . 0)))

  (check-equal?
   (run 1 (q) (multo1 '(S S . 0) '(S S S . 0) q))
   '((S S S S S S . 0)))

  (check-equal?
   (run 1 (q) (multo2 '(S S . 0) '(S S S . 0) q))
   '((S S S S S S . 0)))

  (check-equal?
   (run 1 (q) (lengthdo '(S S 0) '(S S S . 0)))
   '(_.0))

  (check-equal?
   (run 1 (q) (lengtho '(S S 0) '(S S S . 0)))
   '(_.0))

  (check-equal?
   (run 1 (q) (incr-listo '((S S 0) (S S S 0)) q))
   '(((S S S 0) (S S S S 0))))

  (check-equal?
   (run 1 (q) (appendo '(S S 0) '(S S S 0) q))
   '((S S 0 S S S 0)))

  (check-equal?
   (run 1 (q) (appendo q '(S S S 0) '(S S 0 S S S 0)))
   '((S S 0)))


  (check-equal?
   (run 1 (q) (reverseo1 '(S S 0) q))
   '((0 S S)))

  (check-equal?
   (run 1 (q) (reverseo2 '(S S 0) q))
   '((0 S S)))

    (check-equal?
   (run 1 (q) (reverseo1 q '(S S 0)))
   '((0 S S)))

  (check-equal?
   (run 1 (q) (reverseo2 q '(S S 0)))
   '((0 S S)))

  )
