#lang racket/base

(require "../main.rkt"
         racket/list
         racket/port
         (except-in rackunit fail))


;; Printf debugging example

(define (print-out name val)
  (printf "~a: ~a\n" name val)
  (expression-from-goal succeed))

(check-equal?
 (with-output-to-string
   (lambda ()
     (run 1 (q)
       (== q 5)
       (goal-from-expression
        (print-out 'q (expression-from-term q))))))
 "q: 5\n")

(check-equal?
 (with-output-to-string
   (lambda ()
     (run 1 (q)
       (== q 5)
       (goal-from-expression
        (print-out 'q q)))))
 "q: 5\n")

(define-goal-macro spy
  (syntax-rules ()
    [(_ v)
     (goal-from-expression
      (print-out 'v v))]))

(check-equal?
 (with-output-to-string
   (lambda ()
     (run 1 (q)
       (== q 5)
       (spy q))))
 "q: 5\n")

;; Misc...

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
