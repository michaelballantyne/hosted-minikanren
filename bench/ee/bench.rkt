#lang racket

(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")
(require (prefix-in simple: "../ee-stdlib/simple-interp.rkt"))
(require "../utils.rkt")
(require "four-fours.rkt")


(define complex-countdown
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
  (run 9 (b q r) (logo (build-num 68) b q r) (>1o q)))


(module+ main

  (benchmark-suite "numbers"
    ["logo-hard" (logo-hard-program)])

  (benchmark-suite "four-fours"
    ["4" (run 1 (q) (four-fours (build-num 4)))]
    ;; ["256" (run 1 (q) (four-fours (build-num 256)))]
    )

  (benchmark-suite "simple interp"
    ["((\\x x) (\\y y))" (run 1 (q) (simple:evalo '((lambda (x) x) (lambda (y) y)) q))]
    ["complex-countdown" (run 1 (q) (simple:evalo complex-countdown q))]
    )

  ;; (require (prefix-in full: "../ee-stdlib/full-interp.rkt"))
  ;; (benchmark-suite "full interp"
  ;;   ["((\\x x) (\\y y))" (run 1 (q) (full:evalo '((lambda (x) x) (lambda (y) y)) q))])


)
