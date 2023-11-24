#lang racket/base

(require "../main.rkt"
         (except-in rackunit fail))

(check-equal?
 (number? (expression-from-goal (== 'cat 'cat)))
 #f)

(check-equal?
 (run 1 (q) (goal-from-expression (expression-from-goal (== 'cat 'cat))))
 '(_.0))

(check-equal?
 (run 1 (q)
      (goal-from-expression
       (expression-from-goal
        (== 'cat q))))
 '(cat))

(check-equal?
 (run 1 (q)
      (goal-from-expression
       (expression-from-goal
        (fresh (q)
               (goal-from-expression
                (expression-from-goal
                 (== 'cat q)))))))
 '(_.0))

(check-equal?
 (run 1 (q)
      (goal-from-expression
       (expression-from-goal
        (fresh (x)
               (goal-from-expression
                (expression-from-goal
                 (== x q)))))))
 '(_.0))


(define (succeed-or-fail b)
  (if b
      (expression-from-goal (== 'a 'a))
      (expression-from-goal (== 'a 'b))))

(check-equal?
 (run 1 (q)
      (goal-from-expression
       (succeed-or-fail #true)))
 '(_.0))
