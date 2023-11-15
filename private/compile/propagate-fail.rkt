#lang racket/base

(require syntax/parse
         (for-template racket/base
                       "../forms.rkt")
         "utils.rkt"
         "../syntax-classes.rkt")

(provide propagate-fail/entry)

(define CANNOT-DROP 'cannot-drop)

(define (atomic-goal-conjunction? g)
  (syntax-parse g #:literal-sets (mk-literals)
    [c:primitive-goal #true]
    [(c:unary-constraint t) #true]
    [(c:binary-constraint t1 t2) #true]
    [(conj g1 g2) (and (atomic-goal-conjunction? #'g1)
                       (atomic-goal-conjunction? #'g2))]
    [else #false]))

(define (cannot-drop? g)
  (or (syntax-property g CANNOT-DROP)
      (not (atomic-goal-conjunction? g))
      (contains-rkt-term? g)))

;; TODO: This may be quadratic when it could be linear.
(define (propagate-fail/conj g1 g2)
  (let ([g1^ (propagate-fail/goal g1)]
        [g2^ (propagate-fail/goal g2)])
    (syntax-parse #`(conj #,g1^ #,g2^)
      #:literal-sets (mk-literals)
      [(conj fail _) #'fail]
      ;; This pattern is only possible when _ cannot be dropped.
      ;; In that case g1^ will already have the CANNOT-DROP property.
      [(conj (conj _ fail) fail) g1^]
      ;; When g1^ cannot be dropped, but doesn’t have a failure at the
      ;; end already
      [(conj _ fail) #:when (cannot-drop? g1^)
       (syntax-property this-syntax CANNOT-DROP #t)]
      ;; When g1^ can be dropped, therefore we drop it
      [(conj _ fail) #'fail]
      [_ this-syntax])))

(define (propagate-fail/entry g fvs fvs-fresh?)
  (propagate-fail/goal g))

(define (propagate-fail/goal g)
  (syntax-parse g #:literal-sets (mk-literals)
    [c:primitive-goal this-syntax]
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
           (except-in rackunit fail)
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ()
          fail)))
    (generate-prog
      (ir-rel ()
        fail)))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (== (#%lv-ref q) (quote 5))
            fail))))
    (generate-prog
      (ir-rel ((~binder q))
        fail)))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            fail
            (== (#%lv-ref q) (quote 5))))))
    (generate-prog
      (ir-rel ((~binder q))
        fail)))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (conj
              (== (#%lv-ref q) (quote 5))
              fail)
            (== (#%lv-ref p) (quote 6))))))
    (generate-prog
      (ir-rel ((~binders q p))
        fail)))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (conj
              fail
              (== (#%lv-ref q) (quote 5)))
            (== (#%lv-ref p) (quote 6))))))
    (generate-prog
      (ir-rel ((~binders q p))
        fail)))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (conj
              (== (#%lv-ref q) (quote 5))
              (== (#%lv-ref p) (quote 6)))
            fail))))
    (generate-prog
      (ir-rel ((~binders q p))
        fail)))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ()
          (fresh ()
            fail))))
    (generate-prog
      (ir-rel ()
        (fresh ()
          fail))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (== (#%lv-ref q) (#%lv-ref x))
              fail)))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          fail))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (disj
            (== (#%lv-ref q) (quote 5))
            fail))))
    (generate-prog
      (ir-rel ((~binder q))
        (disj
          (== (#%lv-ref q) (quote 5))
          fail))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (disj
            (conj
              (== (#%lv-ref q) (quote 5))
              fail)
            (conj
              fail
              (== (#%lv-ref p) (quote 5)))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (disj
          fail
          fail))))

  (progs-equal?
    (propagate-fail/rel
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (== (#%lv-ref q) (rkt-term 5))
            fail))))
    (generate-prog
      (ir-rel ((~binder q))
        (conj
         (== (#%lv-ref q) (rkt-term 5))
         fail))))

  (progs-equal?
   (propagate-fail/rel
     (generate-prog
       (ir-rel ((~binder q))
         (conj
          (conj (== (rkt-term 5) (#%lv-ref q))
                fail)
          fail))))
   (generate-prog
     (ir-rel ((~binder q))
       (conj (== (rkt-term 5) (#%lv-ref q))
             fail))))

  )
