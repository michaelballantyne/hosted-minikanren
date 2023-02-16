#lang racket

(provide unoptimized-matche-interp optimized-matche-interp)

(require benchmark-minikanren)


(define (optimized-matche-interp)
  (let ((ans (run 1000 (p q) (evalo2 p q))))
    (list* (first ans) '... (last-pair ans))))

(defrel (evalo2 expr val)
  (eval-expro2 expr '() '() val))

(defrel (eval-expro2 expr vars vals val)
  (fresh ()
    (conde
     ((fresh (rator rand)
       (== (cons rator (cons rand '())) expr)
       (fresh (x body vars^ vals^ a)
         (eval-expro2 rator vars vals (cons 'closure (cons x (cons body (cons vars^ (cons vals '()))))))
         (eval-expro2 rand vars vals a)
         (eval-expro2 body (cons x vars^) (cons a vals^) val))))
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
  (let ((ans (run 1000 (p q) (evalo p q))))
    (list* (first ans) '... (last-pair ans))))

(defrel (evalo expr val)
  (eval-expro expr '() '() val))

(defrel (eval-expro expr vars vals val)
  (fresh (ls)
    (== ls (cons expr (cons vars (cons vals (cons val '())))))
    (conde
     ((fresh (rator rand _1 _2 _3)
       (== (cons (cons rator (cons rand '())) (cons _1 (cons _2 (cons _3 '())))) ls)
       (fresh (x body vars^ vals^ a)
         (eval-expro rator vars vals (cons 'closure (cons x (cons body (cons vars^ (cons vals '()))))))
         (eval-expro rand vars vals a)
         (eval-expro body (cons x vars^) (cons a vals^) val))))
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

