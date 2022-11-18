#lang racket/base

(require (for-template racket/base
                       "../forms.rkt")
         ee-lib
         syntax/parse
         (only-in syntax/parse
                  (define/syntax-parse def/stx))
         "../syntax-classes.rkt")

(provide reorder-conj/rel
         reorder-conj/run)

(define (reorder-conj/rel stx)
  (reorder-conjunctions stx))

(define (reorder-conj/run stx)
  (reorder-conjunctions stx))

(define (build-conj l)
  (when (null? l) (error 'build-conj "requires at least one item"))
  (let recur ([l (reverse l)])
    (if (= (length l) 1)
        (car l)
        #`(conj
           #,(recur (cdr l))
           #,(car l)))))

(define (reorder-conjunction stx)
  (define lvars '())
  (define constraints '())
  (define others '())
  (let recur ([stx stx])
    (syntax-parse stx #:literals (conj fresh ==)
                  [(conj g1 g2) (recur #'g1) (recur #'g2)]
                  [(fresh (x:id ...) g)
                   (set! lvars (cons (syntax->list #'(x ...)) lvars))
                   (recur #'g)]
                  [(~or (c:nullary-constraint)
                        (c:unary-constraint t)
                        (c:binary-constraint t1 t2))
                   (set! constraints (cons this-syntax constraints))]
                  [_ (set! others (cons (reorder-conjunctions this-syntax) others))]))
  (let ([lvars (apply append (reverse lvars))]
        [body (build-conj (append (reverse constraints) (reverse others)))])
    (if (null? lvars)
        body
        #`(fresh #,lvars #,body))))

(define (reorder-conjunctions stx)
  (define (maybe-reorder stx)
    (syntax-parse stx
      #:literals (conj fresh)
      [((~or conj fresh) . _) (reorder-conjunction this-syntax)]
      [_ this-syntax]))
  (map-transform maybe-reorder stx))


(module+ test
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))
  
  (progs-equal?
    (reorder-conj/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (success)
            (== (#%lv-ref q) (quote 2))))))
    (generate-prog
      (ir-rel ((~binder q))
        (conj
          (success)
          (== (#%lv-ref q) (quote 2))))))

  )
