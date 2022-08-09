#lang racket

(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")

;; helper function only
(define-relation
 (appendo l s out)
 (conde
  ((== '() l) (== s out))
  ((fresh
    (a d res)
    (== `(,a . ,d) l)
    (== `(,a . ,res) out)
    (appendo d s res)))))

(define-relation
 (fact n n!)
 (conde
  ((== n '()) (== n! '(1)))
  ((fresh
    (n-1)
    (pluso '(1) n-1 n)
    (fresh (n-1!) (fact n-1 n-1!) (*o n n-1! n!))))))

(define-relation
 (fact1 n n!)
 (conde
  ((== n '()) (== n! '(1)))
  ((fresh
    (n-1)
    (pluso '(1) n-1 n)
    (fresh (n-1!) (*o n n-1! n!) (fact1 n-1 n-1!))))))

(define-relation
 (reverse ls sl)
 (conde
  ((== ls '()) (== sl '()))
  ((fresh
    (a d)
    (== ls `(,a . ,d))
    (fresh (b) (reverse d b) (appendo b `(,a) sl))))))

(define-relation
 (reverse1 ls sl)
 (conde
  ((== ls '()) (== sl '()))
  ((fresh
    (a d)
    (== ls `(,a . ,d))
    (fresh (b) (appendo b `(,a) sl) (reverse1 d b))))))

(define-relation
 (mergesorto ls sls)
 (conde
  ((== ls '()) (== sls '()))
  ((fresh
    (a d)
    (== `(,a . ,d) ls)
    (conde
     ((== d '()) (== sls ls))
     ((fresh
       (ad dd)
       (== `(,ad . ,dd) d)
       (fresh
        (l1 l2)
        (msplito dd l1 l2)
        (fresh
         (s1 s2)
         (mergesorto `(,a . ,l1) s1)
         (mergesorto `(,ad . ,l2) s2)
         (mergeo s1 s2 sls))))))))))

(define-relation
 (msplito ls l s)
 (conde
  ((== ls `()) (== `() l) (== `() s))
  ((fresh
    (a d)
    (== `(,a . ,d) ls)
    (conde
     ((== `() d) (== '() s) (== ls l))
     ((fresh
       (ad dd)
       (== `(,ad . ,dd) d)
       (fresh
        (sl1 sl2)
        (== `(,a . ,sl1) l)
        (== `(,ad . ,sl2) s)
        (msplito dd sl1 sl2)))))))))

(define-relation
 (mergeo l s ls)
 (conde
  ((== s '()) (== l ls))
  ((fresh
    (s-a s-d)
    (== `(,s-a . ,s-d) s)
    (conde
     ((== '() l) (== s ls))
     ((fresh
       (l-a l-d)
       (== `(,l-a . ,l-d) l)
       (fresh
        (b)
        (j<=o l-a s-a b)
        (conde
         ((== b #t) (fresh (res) (== `(,l-a . ,res) ls) (mergeo l-d s res)))
         ((== b #f)
          (fresh (res) (== `(,s-a . ,res) ls) (mergeo l s-d res))))))))))))

(define-relation
 (j<=o n m b)
 (conde
  ((== n m) (== b #t))
  ((== '() n) (poso m) (== b #t))
  ((== '(1) n) (>1o m) (== b #t))
  ((== '() m) (poso n) (== b #f))
  ((== '(1) m) (>1o n) (== b #f))
  ((fresh
    (a x b^ y)
    (== `(,a . ,x) m)
    (poso x)
    (== `(,b^ . ,y) n)
    (poso y)
    (conde ((== b #f) (<lo x y)) ((== b #t) (<lo y x)))))
  ((=lo n m)
   (fresh
    (x)
    (poso x)
    (conde ((== b #f) (pluso m x n)) ((== b #t) (pluso n x m)))))))

