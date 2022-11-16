#lang racket

(require "../../mk/mk.rkt")
(require "../../mk/numbers.rkt")
(require (prefix-in simple: "../../mk/simple-interp.rkt"))
(require (prefix-in full: "../../mk/full-interp.rkt"))
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
    ["4" (four-fours 4)]
    ["12-check" (four-fours-at-12-check)]
    ["12" (four-fours 12)]
    ["256" (four-fours 256)])

  (benchmark-suite "simple interp"
    ["((\\x x) (\\y y))" (run 1 (q) (simple:evalo '((lambda (x) x) (lambda (y) y)) q))]
    ["complex-countdown" (run 1 (q) (simple:evalo complex-countdown q))])

  (benchmark-suite "full interp"
    ["((\\x x) (\\y y))" (run 1 (q) (full:evalo '((lambda (x) x) (lambda (y) y)) q))])
)

(module+ test
  (require rackunit)

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
  )
