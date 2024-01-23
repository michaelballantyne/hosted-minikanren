#lang racket
(require "../main.rkt")

(defrel (hand-optimized-evalo expr val)
  (hand-optimized-eval-expro expr '() '() val))

(defrel (hand-optimized-eval-expro expr vars vals val)
  (fresh ()
    (conde
     ((fresh (rator rand)
       (== (cons rator (cons rand '())) expr)
       (fresh (x body vars^ vals^ a)
         (hand-optimized-eval-expro rator vars vals (cons 'closure (cons x (cons body (cons vars^ (cons vals '()))))))
         (hand-optimized-eval-expro rand vars vals a)
         (hand-optimized-eval-expro body (cons x vars^) (cons a vals^) val))))
     ((fresh (x body)
       (== (cons (cons 'lambda (cons (cons x '()) (cons body '())))
                 (cons 'closure (cons x (cons body (cons vars (cons vals '()))))))
           (cons expr val))
       (symbolo x)
       (absento 'lambda vars)))
     ((fresh ()
       (symbolo expr)
       (lookupo2 expr vars vals val))))))

(defrel (lookupo2 x vars vals val)
  (fresh ()
   (conj
    (fresh (y vars^ v vals^)
     (conj
      (== (cons (cons y vars^) (cons (cons v vals^) '())) (cons vars (cons vals '())))
      (conde
       ((conj
		(== y x)
		(== v val)))
       ((conj
		(=/= y x)
		(lookupo2 x vars^ vals^ val)))))))))


(define (unoptimized-matche-interp)
  (let ((ans (run 1000 (p q) (unoptimized-evalo p q))))
    (list* (first ans) '... (last-pair ans))))

(defrel (unoptimized-evalo expr val)
  (unoptimized-eval-expro expr '() '() val))

(defrel (unoptimized-eval-expro expr vars vals val)
  (fresh (ls)
    (== ls (cons expr (cons vars (cons vals (cons val '())))))
    (conde
     ((fresh (rator rand _1 _2 _3)
       (== (cons (cons rator (cons rand '())) (cons _1 (cons _2 (cons _3 '())))) ls)
       (fresh (x body vars^ vals^ a)
         (unoptimized-eval-expro rator vars vals (cons 'closure (cons x (cons body (cons vars^ (cons vals '()))))))
         (unoptimized-eval-expro rand vars vals a)
         (unoptimized-eval-expro body (cons x vars^) (cons a vals^) val))))
     ((fresh (x body vars1 vals1)
       (== (cons (cons 'lambda (cons (cons x '()) (cons body '())))
                 (cons vars1 (cons vals1 (cons (cons 'closure (cons x (cons body (cons vars1 (cons vals1 '()))))) '()))))
           ls)
       (symbolo x)
       (absento 'lambda vars)))
     ((fresh (_1 _2 _3 _4)
       (== (cons _1 (cons _2 (cons _3 (cons _4 '())))) ls)
       (symbolo expr)
       (lookupo expr vars vals val))))))

(defrel (lookupo x vars vals val)
  (fresh (ls)
   (conj
    (== ls (cons vars (cons vals '())))
    (fresh (y vars^ v vals^)
     (conj
      (== (cons (cons y vars^) (cons (cons v vals^) '())) ls)
      (conde
       ((conj
		(== y x)
		(== v val)))
       ((conj
		(=/= y x)
		(lookupo x vars^ vals^ val)))))))))

(namespace-require 'racket)
(namespace-require "../main.rkt")
(namespace-require 'racket/pretty)

#;(define-syntax-rule (print-output-after-optimizations name opts relname rel)
  (begin
    (eval #'(begin-for-syntax (set-optimization-mode! opts)))
    (eval #'rel)
    (eval #'(pretty-print (syntax->datum (relation-code/optimized relname))))))

#;(define-syntax-rule (print-all-optimized-variants relname rel)
  (begin

    (print-output-after-optimizations constant-prop (hash
                                                     'constant-prop #t
                                                     'dead-code #f
                                                     'occurs-check #f
                                                     'unification-spec #f)
                                      relname
                                      rel)

    (print-output-after-optimizations dead-code (hash
                                              'constant-prop #t
                                              'dead-code #t
                                              'occurs-check #f
                                              'unification-spec #f)
                                      relname
                                      rel)

    (print-output-after-optimizations occurs-check (hash
                                              'constant-prop #t
                                              'dead-code #t
                                              'occurs-check #t
                                              'unification-spec #f)
                                      relname
                                      rel)))

#;(print-all-optimized-variants leo2 (defrel/match (leo2 x y)
                                    [(0 ,y)]
                                    [((S . ,x1) (S . ,y1))
                                     (leo2 x1 y1)]))




;; (pretty-print (syntax->datum (relation-code unoptimized-eval-expro)))
;; (pretty-print (syntax->datum (relation-code/optimized unoptimized-eval-expro)))

;; (pretty-print (syntax->datum (relation-code hand-optimized-eval-expro)))
;; (pretty-print (syntax->datum (relation-code/optimized hand-optimized-eval-expro)))
