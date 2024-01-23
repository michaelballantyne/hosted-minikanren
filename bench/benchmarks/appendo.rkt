#lang racket/base

(require benchmark-minikanren racket/list)

(provide standard-appendo-forwards)

;; I assume no inter-relational analysis or ability to change calling convention;
;; thus, no ability to realize we can remove the (=/= xa 'struct) or split out the
;; xsys into two arguments.
(defrel (appendo-manual-internal xs ys zs)
  (conde
    ((== '() xs) (== zs ys))
    ((fresh (xa xd ys zd)
       (== (cons xa xd) xs)
       (== zs (cons xa zd))
       (appendo-manual-internal xd ys zd)))))

(define (standard-appendo-forwards)
  (define l1 (make-list 10000 'x))
  (define l2 (make-list 10000 'y))
  (void (run* (l3) (appendo-manual-internal l1 l2 l3))))

(module+ main
  (time (standard-appendo-forwards))
)
