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
(require "appendo.rkt")
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

  (benchmark-suite "Occurs check"
    ["\\verb|leo| 8000" (dmitri:leo-801)]
    ["\\verb|appendo| w/2 lists" (standard-appendo-forwards)])

  (benchmark-suite "Relational arithmetic"
    ["\\verb|logo|" (logo-hard-program)]
    ["\\verb|four fours| of 256" (four-fours 256)]
    ["\\verb|fact| x = 720" (slow-fact-6-720)])

  (benchmark-suite "Relational interpreters"
    #| "full interp" |#
    #;["complex-countdown 2" (run 1 (q) (full:evalo complex-countdown2 q))]
    ["one quine" (run 4 (q) (full:evalo q q))]
    #| "oxford artifact" |#
    ["99k \\verb|(I love you)|s" (love-in-9900-ways)]
    ["four thrines" (four-thrines)]
    #;["twine-in-standard" (twine-slow)]
    ["dynamic and lexical" (dynamic-then-lexical-3-expressions)]
    #;["lexical then dynamic" (lexical-then-dynamic-3-expressions)]
    ["\\verb|append| synthesis" (append-backward-and-small-synthesis)]
    #| "simple matche-interp" |#
    #;["unoptimized-matche-interp" (unoptimized-matche-interp)]
    #;["optimized-matche-interp" (optimized-matche-interp)]
    #;["scheme-in-scheme-quine-with-quasiquote" (scheme-in-scheme-quine-with-quasiquote)]
    #| "simple interp"|#
    ["countdown from 2 in $\\lambda$-calc" (run 1 (q) (simple:evalo complex-countdown2 q))])

  #;(benchmark-suite "graph coloring"
    #;["color middle earth" (color-middle-earth)]
    ["ways to color iberia" (ways-to-color-iberia)])

  #;(benchmark-suite "disj-dependent"
    ["all-in-fd" (all-in-fd)])

  #;(benchmark-suite "orchid graph coloring"
    ["color kazakhstan" (do-kazakhstan)])

  #;(benchmark-suite "staging-result"
    ["appendo-forwards" (appendo-forwards)]
    ["appendo-forwards-manual" (appendo-forwards-manual)]
    ["appendo-uninstantiated" (appendo-uninstantiated)]
    ["appendo-uninstantiated-manual" (appendo-uninstantiated-manual)]))


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
