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

(test-equal? "list macro expands correctly"
 (test-goal-syntax (fresh (y) (absento (list 'cat 'cat 'cat) y)))
 '(fresh1 (y) (conj (absento (cons 'cat (cons 'cat (cons 'cat '()))) y))))

(test-equal? "Quasiquote and comma work"
 (test-goal-syntax (fresh (y) (absento `(,y fish) y)))
 '(fresh1 (y) (conj (absento (cons y (cons 'fish '())) y))))

(test-equal? "Conj isn't included for a single goal"
 (test-goal-syntax (fresh (x y) (== y x)))
 '(fresh1 (x y) (== y x)))

(defrel (foo x y z)
  (== x y))

(test-equal? "Rel-app invocation seems to work"
 (test-goal-syntax (fresh (y) (foo 'cat 'cat y)))
 '(fresh1 (y) (conj (foo 'cat 'cat y))))

;; This doesn't work the way it should.
(test-equal? "Calling relations seems to work correctly"
 (run 1 (p q r) (foo p q r))
 '((_.0 _.0 _.1)))

(test-equal? "Absento call works"
 (run 1 (q) (absento 'cat q))
 '((_.0 (absento (cat _.0)))))


(require "../../racket-matche.rkt")

(defrel (direct a b)
  (conde
    [(== a 'BOS) (== b 'SEA)]
    [(== a 'HOU) (== b 'SLC)]
    [(== a 'SEA) (== b 'DEN)]
    [(== a 'SEA) (== b 'BOS)]
    [(== a 'DEN) (== b 'HOU)]
    [(== a 'SLC) (== b 'SFO)]))

(defrel (route origin end path)
  (conde
    [(== origin end) (== path '())]
    [(fresh (hop remainder)
       (== path (cons (list origin hop) remainder))
       (absento origin remainder)
       (direct origin hop)
       (route hop end remainder))]))

(run 3 (q) (route 'BOS 'BOS q))

#;(defrel/matche (route-m-macro2 origin end path)
  [(a a '())]
  [(a b (cons (list a layover) remainder))
   (absento a remainder)
   (direct a layover)
   (route-m-macro2 layover b remainder)])
