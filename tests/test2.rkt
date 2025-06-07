#lang racket

(require "../main.rkt")

(defrel (appendo l1 l2 l3)
  (conde
   [(== l1 '()) (== l3 l2)]  ; base case
   [(fresh (head rest) ; recursive case
      (== `(,head . ,rest) l1)
      (fresh (result)
        (== `(,head . ,result) l3)
        (appendo rest l2 result)))]))

(defrel (rel/1-goal-body x)
  (== x 'cat))

(defrel (rel/2-goal-body x y)
  (== x 'fish)
  (== y 'horse))

(defrel (nevero x)
  (nevero x))

(module+ test
  (require
    rackunit
	racket/engine)

  (check-equal?
    (run 2 (q) (appendo `(1 2 3) `(4 5) q))
    '((1 2 3 4 5)))
  (check-equal?
    (run 1 (q) (== (term-from-expression (make-list 5 "a")) q))
    '(("a" "a" "a" "a" "a")))

  (test-equal?
    "defrel permits multi-goal bodies, like faster-minikanren"
	(run* (q) (rel/1-goal-body q))
	'(cat))

  (test-equal?
    "proof we suspend when entering a defrel whose body has implicitly conjoined multiple goals in its body"
	(run* (p q)
	  (conde
		((rel/2-goal-body p q))
		((== p q))))
	'((_.0 _.0) (fish horse)))

  (test-equal?
    "proof we do not suspend when entering a defrel whose body is a single goal"
	(run* (q)
	  (conde
		((rel/1-goal-body q))
		((== q 'zebra))))
	'(cat zebra))

  (test-equal?
   (string-join
	'("We must suspend when entering a defrel whose body is a single goal,"
	  "in case that goal is immediately recurs without shrinking its argument like nevero"))
   (let ([e (engine
			 (lambda (e)
			   (run 1 (q)
				 (conde
				   ((nevero q))
				   ((== q 'zebra))))))])
	 (and (engine-run 1000 e) (engine-result e)))
	'(zebra))

  )

(relation-code appendo)
