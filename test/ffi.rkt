#lang racket/base

(require "../main.rkt"
         rackunit)

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
