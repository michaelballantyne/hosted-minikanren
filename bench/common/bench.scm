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

(define complex-countdown5
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
    (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))

(define (logo-hard-program)
  (let ([N68 (build-num 68)])
    (run 9 (b q r) (logo N68 b q r) (>1o q))))

(module+ main

  (benchmark-suite "numbers"
    ["logo-hard" (logo-hard-program)])

  (benchmark-suite "nolen-example"
    ["nolen-example" (nolen-example)])

  (benchmark-suite "four-fours"
    ["256" (four-fours 256)])

  (benchmark-suite "test fact"
    ["slow fact 7 = 5040" (slow-fact-7-5040)])

  (benchmark-suite "oxford artifact"
    ["love in 99000 ways" (love-in-99000-ways)]
	["four-thrines-small" (four-thrines)]
	["twine-in-standard" (twine-slow)]
	["dynamic-then-lexical-3-expressions" (dynamic-then-lexical-3-expressions)]
	["lexical-then-dynamic-3-expressions" (lexical-then-dynamic-3-expressions)]
	["append-backward-and-small-synthesis" (append-backward-and-small-synthesis)]
	#;["scheme-in-scheme-quine-with-quasiquote" (scheme-in-scheme-quine-with-quasiquote)])

  (benchmark-suite "relational graph coloring"
    ["color middle earth" (color-middle-earth)])

  (benchmark-suite "orchid graph coloring"
    ["color ireland" (do-ireland)])

  (benchmark-suite "simple interp"
    ["((\\x x) (\\y y))" (run 1 (q) (simple:evalo `((lambda (x) x) (lambda (y) y)) q))]
    ["complex-countdown 3" (run 1 (q) (simple:evalo complex-countdown3 q))])

  (benchmark-suite "full interp"
    ["((\\x x) (\\y y))" (run 1 (q) (full:evalo `((lambda (x) x) (lambda (y) y)) q))]
    ["complex-countdown 3" (run 1 (q) (full:evalo complex-countdown3 q))]
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
