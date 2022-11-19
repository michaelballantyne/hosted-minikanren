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
  (let ([N68 (build-num 68)])
    (run 9 (b q r) (logo N68 b q r) (>1o q))))

(module+ main

  (benchmark-suite "numbers"
    ["logo-hard" (logo-hard-program)])

  (benchmark-suite "four-fours"
    ["4" (four-fours 4)]
    ["12-check" (four-fours-at-12-check)]
    ["12" (four-fours 12)]
    ["256" (four-fours 256)])

  (benchmark-suite "simple interp"
    ["((\\x x) (\\y y))" (run 1 (q) (simple:evalo `((lambda (x) x) (lambda (y) y)) q))]
    ["complex-countdown" (run 1 (q) (simple:evalo complex-countdown q))])

  (benchmark-suite "full interp"
    ["((\\x x) (\\y y))" (run 1 (q) (full:evalo `((lambda (x) x) (lambda (y) y)) q))]
    ["complex-countdown" (run 1 (q) (full:evalo complex-countdown q))]
    ["6 quines" (run 6 (q) (full:evalo q q))]))

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