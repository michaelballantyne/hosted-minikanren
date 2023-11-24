#lang racket/base
(require ee-lib/errors
         racket/math
         (for-syntax racket/base)
         racket/stxparam
         (prefix-in mku: "../mk/private-unstable.rkt"))

(provide (all-defined-out))

; Runtime

(struct relation-value [proc])
(struct goal-value [proc])
(struct sealed-lvar [var])

(define (check-natural val blame-stx)
  (if (natural? val)
      val
      (raise-argument-error/stx 'run "natural?" val blame-stx)))

(define (check-relation val blame-stx)
  (if (relation-value? val)
      val
      (raise-argument-error/stx
       'apply-relation
       "relation-value?"
       val
       blame-stx)))

(define (check-and-unseal-goal val blame-stx)
  (if (goal-value? val)
      (goal-value-proc val)
      (raise-argument-error/stx
       'goal-from-expression
       "goal-value?"
       val
       blame-stx)))

(define (mk-value? v)
  (or (symbol? v)
      (string? v)
      (number? v)
      (null? v)
      (boolean? v)
      (sealed-lvar? v)
      (and (pair? v)
           (mk-value? (car v))
           (mk-value? (cdr v)))))

(define (unseal-vars-in-term v blame-stx)
  (cond
    [(or (symbol? v)
         (string? v)
         (number? v)
         (null? v)
         (boolean? v))
     v]
    [(sealed-lvar v) (sealed-lvar-var v)]
    [(pair? v) (cons (unseal-vars-in-term (car v) blame-stx)
                     (unseal-vars-in-term (cdr v) blame-stx))]
    [else (raise-argument-error/stx 'term "mk-value?" v blame-stx)]))

(define check-constraints 
  (lambda (S^ added st)
    (if S^
        (mku:and-foldl mku:update-constraints (mku:state S^ (mku:state-C st)) added)
        #f)))

(define-syntax-parameter surrounding-term-vars-in-scope '())
(define-syntax-parameter surrounding-current-state-var #'mku:empty-state)

(define (seal-vars-in-term t)
  (cond
    [(pair? t) (cons (seal-vars-in-term (car t))
                     (seal-vars-in-term (cdr t)))]
    [(mku:var? t) (sealed-lvar t)]
    [else t])
  )
