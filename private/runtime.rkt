#lang racket/base
(require ee-lib/errors
         racket/math
         (prefix-in mku: "../mk/private-unstable.rkt"))

(provide (all-defined-out))

; Runtime

(struct relation-value [proc])

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

(define (mk-value? v)
  (or (symbol? v)
      (string? v)
      (number? v)
      (null? v)
      (boolean? v)
      (and (pair? v)
           (mk-value? (car v))
           (mk-value? (cdr v)))))

(define (check-term val blame-stx)
  (if (mk-value? val)
      val
      (raise-argument-error/stx 'term "mk-value?" val blame-stx)))

(define check-constraints 
  (lambda (S^ added st)
    (if S^
        (mku:and-foldl mku:update-constraints (mku:state S^ (mku:state-C st)) added)
        #f)))
