#lang racket/base

(require benchmark-minikanren)

(provide 100-appendo 100-appendo-manual)

;; A benchmark produced by staging the following in staged minikanren,
;; manually fixed up a bit to fit within the language of minikanren-ee.

#;(defrel (appendo xs ys zs)
  (staged
   (evalo-staged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs)
                     ys
                     (cons (car xs)
                           (append (cdr xs) ys))))))
       (append ',xs ',ys))
    zs)))


(defrel (not-tago/gen v)
  (=/= 'struct v))

(defrel (appendo xs62 ys63 zs64)
  (fresh
      (_.0 _.1 _.2 _.3 _.22 _.23)
    (absento 'struct _.0)
    (=/= _.0 'struct)
    (absento 'struct _.1)
    (=/= _.1 'struct)
    (== _.0 xs62)
    (== _.1 ys63)
    (== _.2 zs64)
    (== _.22 _.0)
    (== _.23 _.1)
    (appendo-internal (list _.22 _.23) _.2)))

(defrel (appendo-internal _38 _39)
  (fresh
      (_.6
       _.5
       _.4
       _.7
       _.8
       _.9
       _.10
       _.11
       _.15
       _.14
       _.12
       _.13
       _.17
       _.16
       _.21
       _.18
       _.19
       _.20)
    (== _.4 (cons _.5 _.6))
    (== (list _.5 _.7) _38)
    (== _.8 _39)
    (== (list _.9) (list _.10))
    (conde ((== '() _.9) (== '#t _.11)) ((=/= '() _.9) (== '#f _.11)))
    (== _.5 _.10)
    (conde
      ((=/= '#f _.11) (== _.7 _.8))
      ((== '#f _.11)
       (== (list _.12 _.13) (list _.14 _.15))
       (== (cons _.12 _.13) _.8)
       (== (list (cons _.14 _.16)) (list _.17))
       (not-tago/gen _.14)
       (== _.5 _.17)
       (== (list (cons _.18 _.19)) (list _.20))
       (not-tago/gen _.18)
       (== _.5 _.20)
       (== _.7 _.21)
       (appendo-internal (list _.19 _.21) _.15)))))


;; A manually-written version that is what I imagine we should be able to optimize the
;; above to, keeping the same calling conventions and external constraints forced by
;; the structure of the interpreter it is staged from.
(defrel (appendo-manual xs ys zs)
  (fresh ()
    ;; The absento and =/= struct constraints come from the way the example was generated
    ;; by staging evaluation of quotation forms in the interpreter.
    (absento 'struct xs)
    (absento 'struct ys)
    (appendo-manual-internal (list xs ys) zs)))

;; I assume no inter-relational analysis or ability to change calling convention;
;; thus, no ability to realize we can remove the (=/= xa 'struct) or split out the
;; xsys into two arguments.
(defrel (appendo-manual-internal xsys zs)
  (conde
    ((== (list '() zs) xsys))
    ((fresh (xa xd ys zd)
       (== (list (cons xa xd) ys) xsys)
       (=/= xa 'struct)
       (== zs (cons xa zd))
       (appendo-manual-internal (list xd ys) zd)))))

(define (100-appendo)
  (void (run 100 (l1 l2 l3) (appendo l1 l2 l3))))

(define (100-appendo-manual)
  (void (run 100 (l1 l2 l3) (appendo-manual l1 l2 l3))))