#lang racket/base

(require syntax/parse
         (for-template racket/base
                       "../forms.rkt")
         "utils.rkt"
         "../syntax-classes.rkt")

(provide propagate-fail/rel
         propagate-fail/run)

(define (propagate-fail/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     #`(ir-rel (x ...) #,(propagate-fail/goal #'g))]))

(define (propagate-fail/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     #`(run n (q ...) #,(propagate-fail/goal #'g))]
    [(run* (q ...) g)
     #`(run* (q ...) #,(propagate-fail/goal #'g))]))

(define HAS-RKT-TERM 'contains-rkt-term)

(define (contains-rkt-term/cache g)
  (or (syntax-property g HAS-RKT-TERM)
      (contains-rkt-term? g)))

(define (propagate-fail/conj g1 g2)
  (let ([g1^ (propagate-fail/goal g1)]
        [g2^ (propagate-fail/goal g2)])
    (syntax-parse #`(conj #,g1^ #,g2^)
      #:literal-sets (mk-literals)
      [(conj (failure) _) #'(failure)]
      [(conj _ (failure)) #:when (not (contains-rkt-term/cache g1^))
       (if (contains-rkt-term/cache g1^)
         (syntax-property this-syntax HAS-RKT-TERM #t)
         #'(failure))]
      ;; the syntax property prevents exponential blowup
      ;; in left-associative conj-es containing a rkt-term
      [_ this-syntax])))

(define (propagate-fail/goal g)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:nullary-constraint) this-syntax]
    [(c:unary-constraint t) this-syntax]
    [(c:binary-constraint t1 t2) this-syntax]
    [(conj g1 g2) (propagate-fail/conj #'g1 #'g2)]
    [(disj g1 g2)
     #`(disj #,(propagate-fail/goal #'g1)
             #,(propagate-fail/goal #'g2))]
    [(fresh (x ...) g)
     #`(fresh (x ...)
         #,(propagate-fail/goal #'g))]
    [(#%rel-app n t ...) this-syntax]
    [(apply-relation e t ...) this-syntax]))

(module+ test
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ()
          (failure))))
    (generate-prog
      (ir-rel ()
        (failure))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (== (#%lv-ref q) (quote 5))
            (failure)))))
    (generate-prog
      (ir-rel ((~binder q))
        (failure))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (failure)
            (== (#%lv-ref q) (quote 5))))))
    (generate-prog
      (ir-rel ((~binder q))
        (failure))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (conj
              (== (#%lv-ref q) (quote 5))
              (failure))
            (== (#%lv-ref p) (quote 6))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (failure))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (conj
              (failure)
              (== (#%lv-ref q) (quote 5)))
            (== (#%lv-ref p) (quote 6))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (failure))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (conj
              (== (#%lv-ref q) (quote 5))
              (== (#%lv-ref p) (quote 6)))
            (failure)))))
    (generate-prog
      (ir-rel ((~binders q p))
        (failure))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ()
          (fresh ()
            (failure)))))
    (generate-prog
      (ir-rel ()
        (fresh ()
          (failure)))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (== (#%lv-ref q) (#%lv-ref x))
              (failure))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (failure)))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (disj
            (== (#%lv-ref q) (quote 5))
            (failure)))))
    (generate-prog
      (ir-rel ((~binder q))
        (disj
          (== (#%lv-ref q) (quote 5))
          (failure)))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (disj
            (conj
              (== (#%lv-ref q) (quote 5))
              (failure))
            (conj
              (failure)
              (== (#%lv-ref p) (quote 5)))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (disj
          (failure)
          (failure)))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (== (#%lv-ref q) (rkt-term 5))
            (failure)))))
    (generate-prog
      (ir-rel ((~binder q))
        (conj
         (== (#%lv-ref q) (rkt-term 5))
         (failure)))))

  )
