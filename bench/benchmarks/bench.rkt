#lang racket

(require benchmark-minikanren)
(require "stdlib/numbers.rkt")
(require (prefix-in simple: "stdlib/simple-interp.rkt"))
(require (prefix-in full: "stdlib/full-interp.rkt"))
(require "../utils.rkt")
(require "four-fours.rkt")
(require "all-in-fd.rkt")
(require "test-fact.rkt")
(require "relational-graph-color.rkt")
(require "graph-coloring/orchid-graph-coloro.rkt")
(require "simple-interp-matche-compare.rkt")
(require (prefix-in dmitri: "dmitri-oc-test.rkt"))
(require "icfp2017/challenge-1.rkt")
(require "icfp2017/challenge-2.rkt")
(require "icfp2017/challenge-2-extra-slow.rkt")
(require "icfp2017/challenge-3.rkt")
(require "icfp2017/challenge-4.rkt")
(require "icfp2017/challenge-7.rkt")
(require "staging-result.rkt")


(define complex-countdown3
  '(((lambda (w) (w w))
     (lambda (f)
       (lambda (n)
         ((lambda (id)
            ((n (lambda (_)
                  ((f f) (lambda (f)
                           (lambda (x)
                             (((n (lambda (g)
                                    (lambda (h)
                                      (h (g f)))))
                               (lambda (u) x))
                              id))))))
             id))
          (lambda (f) f)))))
    (lambda (f) (lambda (x) (f (f (f x)))))))

(define complex-countdown2
  '(((lambda (w) (w w))
     (lambda (f)
       (lambda (n)
         ((lambda (id)
            ((n (lambda (_)
                  ((f f) (lambda (f)
                           (lambda (x)
                             (((n (lambda (g)
                                    (lambda (h)
                                      (h (g f)))))
                               (lambda (u) x))
                              id))))))
             id))
          (lambda (f) f)))))
    (lambda (f) (lambda (x) (f (f x))))))

(define (logo-hard-program)
  (let ([N68 (build-num 68)])
    (run 9 (b q r) (logo N68 b q r) (>1o q))))

(module+ main

  (benchmark-suite "numbers"
    ["logo-hard" (logo-hard-program)])

  (benchmark-suite "all-in-fd"
    ["all-in-fd" (all-in-fd)])

  (benchmark-suite "four-fours"
    ["256" (four-fours 256)])

  (benchmark-suite "test fact"
    ["slow fact 6 = 720" (slow-fact-6-720)])

  (benchmark-suite "oxford artifact"
    ["love in 9900 ways" (love-in-9900-ways)]
    ["four-thrines-small" (four-thrines)]
    #;["twine-in-standard" (twine-slow)]
    ["dynamic-then-lexical-3-expressions" (dynamic-then-lexical-3-expressions)]
    ["lexical-then-dynamic-3-expressions" (lexical-then-dynamic-3-expressions)]
    ["append-backward-and-small-synthesis" (append-backward-and-small-synthesis)]
    #;["scheme-in-scheme-quine-with-quasiquote" (scheme-in-scheme-quine-with-quasiquote)])

  (benchmark-suite "dmitri oc bench check"
    ["dmitri leo 8000" (dmitri:leo-801)])

  (benchmark-suite "relational graph coloring"
    #;["color middle earth" (color-middle-earth)]
    ["ways to color iberia" (ways-to-color-iberia)])

  (benchmark-suite "orchid graph coloring"
    ["color kazakhstan" (do-kazakhstan)])

  (benchmark-suite "simple interp"
    ["complex-countdown 2" (run 1 (q) (full:evalo complex-countdown2 q))])

  (benchmark-suite "simple matche-interp"
    ["unoptimized-matche-interp" (unoptimized-matche-interp)]
    ["optimized-matche-interp" (optimized-matche-interp)])

  (benchmark-suite "full interp"
    ["complex-countdown 2" (run 1 (q) (full:evalo complex-countdown2 q))]
    ["1 real quine" (run 4 (q) (full:evalo q q))])

  (benchmark-suite "staging-result"
    ["100 appendo" (100-appendo)]
    ["100 appendo baseline" (100-appendo-manual)]))


(module+ test
  (require (except-in rackunit fail))

  (require (submod "test-fact.rkt" test)
           (submod "four-fours.rkt" test)
           (submod "dmitri-oc-test.rkt" test)
           (submod "relational-graph-color.rkt" test)
           (submod "infer.rkt" test)
           (submod "icfp2017/challenge-1.rkt" test)
           (submod "icfp2017/challenge-4.rkt" test))

  (check-equal?
   (logo-hard-program)
   '((() (_.0 _.1 . _.2) (0 0 1 0 0 0 1))
     ((1) (_.0 _.1 . _.2) (1 1 0 0 0 0 1))
     ((0 1) (0 1 1) (0 0 1))
     ((1 1) (1 1) (1 0 0 1 0 1))
     ((0 0 1) (1 1) (0 0 1))
     ((0 0 0 1) (0 1) (0 0 1))
     ((1 0 1) (0 1) (1 1 0 1 0 1))
     ((0 1 1) (0 1) (0 0 0 0 0 1))
     ((1 1 1) (0 1) (1 1 0 0 1))))

  (check-equal?
   (run 1 (q) (simple:evalo '((lambda (x) x) (lambda (y) y)) q))
   '((closure y y ())))

  (check-equal?
   (unoptimized-matche-interp)
   (optimized-matche-interp))

  )
