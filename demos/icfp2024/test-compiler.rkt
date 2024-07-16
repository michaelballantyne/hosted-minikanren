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
 'cat)

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
