#lang racket
(require (except-in rackunit fail))
(require "./minikanren-spec-compiler.rkt")

(check-equal?
 (run 1 (x) (== x x))
 '(_.0))

(check-equal?
 (run 1 (x) (== x (quote cat)))
 '(cat))

(check-equal?
 (run 1 (q) (== q 'cat))
 '(cat))

(check-equal?
 (run 1 (q) (== q (term-from-expression 'cat)))
 '(cat))

(check-equal?
 (run 1 (q) (== q (term-from-expression (expression-from-term 'cat))))
 '(cat))

(check-equal?
 (run 1 (q) (goal-from-expression (expression-from-goal succeed)))
 '(_.0))

(check-equal?
 (with-output-to-string
   (λ ()
     (displayln
      (run 1 (q)
        (goal-from-expression
         (let ()
           (printf "this is a goal: ~s:\n" (expression-from-goal succeed))
           (expression-from-goal succeed)))))))
 "this is a goal: #<mk-goal>:\n(_.0)\n")

(check-exn
 #rx"mk-goal?"
 (λ ()
   (run 1 (q)
     (goal-from-expression
      (λ (st) st)))))

(check-equal?
 (with-output-to-string
   (λ ()
     (displayln
      (run 1 (q)
        (fresh (x)
          (== q (list x x))
          (goal-from-expression
           (let ()
             (printf "q is ~s:\n" (expression-from-term q))
             (expression-from-goal succeed))))))))
 "q is (#<mk-lvar> #<mk-lvar>):\n((_.0 _.0))\n")

(test-equal? "list macro expands correctly"
 (test-goal-syntax (fresh (y) (absento (list 'cat 'cat 'cat) y)))
 '(fresh1 (y)
    (conj (absento
       (cons
        (core-quote cat)
        (cons (core-quote cat) (cons (core-quote cat) (core-quote ()))))
       y))))

(test-equal? "Quasiquote and comma work"
 (test-goal-syntax (fresh (y) (absento `(,y fish) y)))
 '(fresh1 (y)
    (conj (absento (cons y (cons (core-quote fish) (core-quote ()))) y))))

(test-equal? "Conj is included for a single goal"
 (test-goal-syntax (fresh (x y) (== y x)))
 '(fresh1 (x y) (conj (== y x))))

(defrel (foo x y z)
  (== x y))

(test-equal? "Rel-app invocation seems to work"
 (test-goal-syntax (fresh (y) (foo 'cat 'cat y)))
 '(fresh1 (y) (conj (foo (core-quote cat) (core-quote cat) y))))

;; This doesn't work the way it should.
(test-equal? "Calling relations seems to work correctly"
 (run 1 (p q r) (foo p q r))
 '((_.0 _.0 _.1)))

(test-equal? "Absento call works"
 (run 1 (q) (absento 'cat q))
 '((_.0 (absento (cat _.0)))))
