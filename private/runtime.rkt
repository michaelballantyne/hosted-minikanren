#lang racket/base
(require syntax-spec/private/ee-lib/errors
         racket/math
         (for-syntax racket/base)
         racket/stxparam
         (prefix-in mku: "../mk/private-unstable.rkt"))

(provide (all-defined-out))

; Runtime

(struct relation-value [proc])
(struct mk-goal [proc])
(struct mk-lvar [var]
  #:methods gen:equal+hash
  [(define (equal-proc this other rec)
     (eq? (mk-lvar-var this) (mk-lvar-var other)))
   (define (hash-proc this rec)
     (rec (mk-lvar-var this)))
   (define (hash2-proc this rec)
     (rec (mk-lvar-var this)))])

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

(define (seal-goal g)
  (mk-goal g))

(define (unseal-and-apply-goal val blame-stx state)
  (if (mk-goal? val)
      ((mk-goal-proc val) state)
      (raise-argument-error/stx
       'goal-from-expression
       "goal-value?"
       val
       blame-stx)))

(define (mk-atom? t)
  (or (string? t)
      (symbol? t)
      (number? t)
      (boolean? t)))

(define (mk-value? v)
  (or (null? v)
      (mk-atom? v)
      (mk-lvar? v)
      (and (pair? v)
           (mk-value? (car v))
           (mk-value? (cdr v)))))

(define (check-and-unseal-vars-in-term v blame-stx)
  (cond
    [(or (symbol? v)
         (string? v)
         (number? v)
         (null? v)
         (boolean? v))
     v]
    [(mk-lvar? v) (mk-lvar-var v)]
    [(pair? v) (cons (check-and-unseal-vars-in-term (car v) blame-stx)
                     (check-and-unseal-vars-in-term (cdr v) blame-stx))]
    [else (raise-argument-error/stx 'term "mk-value?" v blame-stx)]))

(define (expression-from-term-rt t st)
  (seal-vars-in-term (mku:walk* t (mku:state-S st))))

(define (unify2/no-occur-check/check-unseal-vars v e e-stx st)
  (mku:unify2-no-occur-check v (check-and-unseal-vars-in-term e e-stx) st))

(define (unify2/check-unseal-vars v e e-stx st)
  (mku:unify2 v (check-and-unseal-vars-in-term e e-stx) st))

(define-syntax-parameter surrounding-term-vars-in-scope '())
(define-syntax-parameter surrounding-current-state-var #'mku:empty-state)

(define (seal-vars-in-term t)
  (cond
    [(pair? t) (cons (seal-vars-in-term (car t))
                     (seal-vars-in-term (cdr t)))]
    [(mku:var? t) (mk-lvar t)]
    [else t])
  )

;; Compiler runtime helpers
(define (get-state-from-scope st)
  (mku:subst-scope (mku:state-S st)))

(define (fresh-var-w-state-scope st)
  (mku:var (get-state-from-scope st)))

(define (walk-in-state v st)
  (mku:walk v (mku:state-S st)))

(define (extend-s-w/no-check w v st)
  (mku:ext-st-no-check w (walk-in-state v st) st))

(define (check-constraints S^ added st)
  (if S^
      (mku:and-foldl mku:update-constraints (mku:state S^ (mku:state-C st)) added)
      #f))
