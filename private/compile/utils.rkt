#lang racket/base

(require syntax/parse
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt")

(provide contains-rkt-term?)

(define (contains-rkt-term? g)
  (define (goal-contains? g)
    (syntax-parse g
      #:literal-sets (mk-literals)
      [(c:nullary-constraint) #f]
      [(c:unary-constraint t)
       (term-contains? #'t)]
      [(c:binary-constraint t1 t2)
       (or (term-contains? #'t1)
           (term-contains? #'t2))]
      [(conj g1 g2)
       (or (goal-contains? #'g1)
           (goal-contains? #'g2))]
      [(disj g1 g2)
       (or (goal-contains? #'g1)
           (goal-contains? #'g2))]
      [(fresh (x ...) g) (goal-contains? #'g)]
      [(#%rel-app n t ...)
       (ormap term-contains? (syntax->list #'(t ...)))]
      [(#%apply-relation e t ...)
       (ormap term-contains? (syntax->list #'(t ...)))]))

  (define (term-contains? t)
    (syntax-parse t
      #:literal-sets (mk-literals)
      #:literals (quote cons)
      [(quote _) #f]
      [(#%lv-ref _) #f]
      [(rkt-term _) #t]
      [(cons t1 t2)
       (or (contains-rkt-term? #'t1)
           (contains-rkt-term? #'t2))]))

  (goal-contains? g))

