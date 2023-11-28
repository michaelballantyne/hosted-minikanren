#lang racket/base

(require "../main.rkt"
         racket/list
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

(define (real-succeed-or-fail b)
  (if b
      (expression-from-goal succeed)
      (expression-from-goal fail)))

(check-equal?
 (run 1 (q)
      (goal-from-expression
       (real-succeed-or-fail #true)))
 '(_.0))



(check-equal?
 (run 1 (q) (== (term-from-expression (expression-from-term q)) 'cat))
 '(cat))




(check-equal?
 (run 1 (q) (== (term-from-expression
                 (expression-from-term
                  (term-from-expression
                   (expression-from-term q))))
                'cat))
 '(cat))



(check-equal?
 (run 1 (q)
      (fresh (y)
             (== q
                 (term-from-expression
                  (make-list
                   3
                   (expression-from-term (cons y 5)))))))
 '(((_.0 . 5) (_.0 . 5) (_.0 . 5))))

(check-equal?
 (run 1 (q) (== (term-from-expression
                 (cons
                  (expression-from-term
                   (term-from-expression
                    (expression-from-term q)))
                  'fish))
                'cat))
 '())

(check-equal?
 (run 1 (q)
      (fresh (x)
       (== x 'cat)
       (== (term-from-expression
            (cons
             (expression-from-term
              (term-from-expression
               (expression-from-term x)))
             'fish))
           q)))
 '((cat . fish)))

(define (cat-or-dog b tv)
  (if b
      (expression-from-goal (== (term-from-expression tv) 'cat))
      (expression-from-goal (== (term-from-expression tv) 'dog))))

(check-equal?
 (run 1 (q)
      (goal-from-expression
       (cat-or-dog #true (expression-from-term q))))
 '(cat))
