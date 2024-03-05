#lang racket/base

(require syntax/parse
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt")

(provide contains-term-from-expression?)

(define (contains-term-from-expression? g)
  (define (goal-contains? g)
    (syntax-parse g
      #:literal-sets (mk-literals)
      [c:primitive-goal #f]
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
      [(goal-from-expression e) #t]
      [(#%apply-relation e t ...)
       (ormap term-contains? (syntax->list #'(t ...)))]))

  (define (term-contains? t)
    (syntax-parse t
      #:literal-sets (mk-literals)
      #:literals (quote cons)
      [(quote _) #f]
      [(#%lv-ref _) #f]
      [(term-from-expression _) #t]
      [(cons t1 t2)
       (or (term-contains? #'t1)
           (term-contains? #'t2))]))

  (goal-contains? g))

