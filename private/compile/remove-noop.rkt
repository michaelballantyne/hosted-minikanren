#lang racket/base

(require syntax/parse
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt")

(provide remove-noop/rel
         remove-noop/run)

(define (remove-noop/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     #`(ir-rel (x ...) #,(remove-noop/goal #'g))]))

(define (remove-noop/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     #`(run n (q ...) #,(remove-noop/goal #'g))]
    [(run* (q ...) g)
     #`(run* (q ...) #,(remove-noop/goal #'g))]))

(define (remove-noop/goal g)
  (syntax-parse g #:literal-sets (mk-literals)
    [c:primitive-goal this-syntax]
    [(c:unary-constraint t) this-syntax]
    [(c:binary-constraint t1 t2) this-syntax]
    [(conj g1 g2)
     (with-syntax ([g1^ (remove-noop/goal #'g1)]
                   [g2^ (remove-noop/goal #'g2)])
       (syntax-parse (list #'g1^ #'g2^) #:literal-sets (mk-literals)
         [(succeed g2) #'g2]
         [(g1 succeed) #'g1]
         [(g1 g2) #'(conj g1 g2)]))]
    [(disj g1 g2)
     #`(disj #,(remove-noop/goal #'g1)
             #,(remove-noop/goal #'g2))]
    [(fresh (x ...) g)
     #`(fresh (x ...)
         #,(remove-noop/goal #'g))]
    [(#%rel-app n t ...) this-syntax]
    [(apply-relation e t ...) this-syntax]))

(module+ test
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           (except-in rackunit fail)
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ()
          succeed)))
    (generate-prog
      (ir-rel ()
        succeed)))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (== (#%lv-ref q) (quote 5))
            succeed))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (#%lv-ref q) (quote 5)))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            succeed
            (== (#%lv-ref q) (quote 5))))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (#%lv-ref q) (quote 5)))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ()
          (fresh ()
            succeed))))
    (generate-prog
      (ir-rel ()
        (fresh ()
              succeed))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (conj
              (== (#%lv-ref q) (quote 5))
              succeed)
            succeed))))
    (generate-prog
      (ir-rel ((~binders q p))
        (== (#%lv-ref q) (quote 5)))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (== (#%lv-ref q) (quote 5))
            (conj
              succeed
                (== (#%lv-ref p) (quote 6)))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (conj
          (== (#%lv-ref q) (quote 5))
          (== (#%lv-ref p) (quote 6))))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            succeed
            (conj
              succeed
              (== (#%lv-ref q) (quote 5)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (#%lv-ref q) (quote 5)))))

  ;; Should be able to remove successes from nested calls
  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (conj
              succeed
              succeed)
            (== (#%lv-ref q) (quote 5))))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (#%lv-ref q) (quote 5)))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (== (#%lv-ref q) (#%lv-ref x))
              succeed)))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (== (#%lv-ref q) (#%lv-ref x))))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              succeed
              (== (#%lv-ref q) (#%lv-ref x)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (== (#%lv-ref q) (#%lv-ref x))))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (disj
            succeed
            succeed))))
    (generate-prog
      (ir-rel ((~binder q))
        (disj
          succeed
          succeed))))

  (progs-equal?
    (remove-noop/rel
      (generate-prog
        (ir-rel ((~binder q))
          (disj
            (conj succeed (== (#%lv-ref q) (quote 5)))
            (conj (== (#%lv-ref q) (quote 6)) succeed)))))
    (generate-prog
      (ir-rel ((~binder q))
        (disj
          (== (#%lv-ref q) (quote 5))
          (== (#%lv-ref q) (quote 6))))))

  )
