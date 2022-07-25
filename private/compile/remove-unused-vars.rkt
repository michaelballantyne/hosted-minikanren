#lang racket/base

(require syntax/parse
         (only-in racket/sequence in-syntax)
         (for-template racket/base "../forms.rkt")
         "../syntax-classes.rkt"
         syntax/id-set)

(provide remove-unused-vars/rel
         remove-unused-vars/run)

(define (remove-unused-vars/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     (let-values ([(g^ _) (remove-unused-vars #'g)])
       #`(ir-rel (x ...) #,g^))]))

(define (remove-unused-vars/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let-values ([(g^ _) (remove-unused-vars #'g)])
       #`(run n (q ...) #,g^))]
    [(run* (q ...) g)
     (let-values ([(g^ _) (remove-unused-vars #'g)])
       #`(run* (q ...) #,g^))]))

;; produce a new goal where only referenced logic variables get freshened
;; and a set of referenced free identifiers.
;; (-> goal? (values goal? immutable-free-id-set?))
(define (remove-unused-vars g)
  (syntax-parse g
    #:literal-sets (mk-literals)
    [(c:nullary-constraint) (values this-syntax (immutable-free-id-set))]
    [(c:unary-constraint t) (values this-syntax (term-refs #'t))]
    [(c:binary-constraint t1 t2) (values this-syntax (free-id-set-union (term-refs #'t1) (term-refs #'t2)))]
    [(disj g1 g2)
     (let-values ([(g1^ g1-refs) (remove-unused-vars #'g1)]
                  [(g2^ g2-refs) (remove-unused-vars #'g2)])
       (values #`(disj #,g1^ #,g2^) (free-id-set-union g1-refs g2-refs)))]
    [(conj g1 g2)
     (let-values ([(g1^ g1-refs) (remove-unused-vars #'g1)]
                  [(g2^ g2-refs) (remove-unused-vars #'g2)])
       (values #`(conj #,g1^ #,g2^) (free-id-set-union g1-refs g2-refs)))]
    [(fresh (x ...) g)
     (let-values ([(g^ g-refs) (remove-unused-vars #'g)])
       (define vars-to-keep (filter (λ (lv) (free-id-set-member? g-refs lv)) (syntax->list #'(x ...))))
       (define free-refs (free-id-set-subtract g-refs (immutable-free-id-set vars-to-keep)))
       (values #`(fresh (#,@vars-to-keep) #,g^) free-refs))]
    [(#%rel-app n t ...)
     (values this-syntax
             (for/fold ([var-refs (immutable-free-id-set)])
                       ([t (in-syntax #'(t ...))])
               (free-id-set-union var-refs (term-refs t))))]
    [(apply-relation e t ...)
     (values this-syntax
             (for/fold ([var-refs (immutable-free-id-set)])
                       ([t (in-syntax #'(t ...))])
               (free-id-set-union var-refs (term-refs t))))]))

(define (term-refs t)
  (syntax-parse t #:literal-sets (mk-literals) #:literals (cons quote)
    [(#%lv-ref v)
     (immutable-free-id-set (list #'v))]
    [(rkt-term _) (immutable-free-id-set)]
    [(#%term-datum _) (immutable-free-id-set)]
    [(quote _) (immutable-free-id-set)]
    [(cons t1 t2)
     (free-id-set-union (term-refs #'t1) (term-refs #'t2))]))

(module* test racket/base
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ((~binders a))
          (fresh ((~binders x y))
            (== (#%lv-ref x) (#%lv-ref a))))))
    (generate-prog
      (ir-rel ((~binders a))
        (fresh ((~binders x))
          (== (#%lv-ref x) (#%lv-ref a))))))

  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ()
          (fresh ((~binder a))
            (== (#%lv-ref a) (#%term-datum 5))))))
    (generate-prog
      (ir-rel ()
        (fresh ((~binder a))
          (== (#%lv-ref a) (#%term-datum 5))))))

  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ()
          (disj
            (fresh ((~binder x))
              (== (#%lv-ref x) (#%lv-ref x)))
            (fresh ((~binder y))
              (== (#%term-datum 5) (#%term-datum 6)))))))

    (generate-prog
      (ir-rel ()
        (disj
          (fresh ((~binder x))
            (== (#%lv-ref x) (#%lv-ref x)))
          (fresh ()
            (== (#%term-datum 5) (#%term-datum 6)))))))

  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (fresh ((~binder y))
              (conj
                (== (#%lv-ref x) (#%term-datum 5))
                (fresh ((~binder z))
                  (== (#%lv-ref z) (#%lv-ref y)))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
              (fresh ((~binder y))
                (conj
                  (== (#%lv-ref x) (#%term-datum 5))
                  (fresh ((~binder z))
                    (== (#%lv-ref z) (#%lv-ref y)))))))))

  )
