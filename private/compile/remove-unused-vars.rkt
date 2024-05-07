#lang racket/base

(require syntax/parse
         ee-lib
         (only-in racket/sequence in-syntax)
         (for-template racket/base "../forms.rkt")
         (only-in "prop-vars.rkt" TERM-VARS-IN-SCOPE)
         "../syntax-classes.rkt"
         syntax/id-set)

(provide remove-unused-vars/entry)

(define (remove-unused-vars/entry g fvs fvs-fresh?)
  (let-values ([(g^ _) (remove-unused-vars g fvs)])
    g^))

;; produce a new goal where only referenced logic variables get freshened
;; and a set of referenced free identifiers.
;; EFFECT annotates goal from expression form w/syntax property listing vars in scope at that expression
;; (goal? [Listof identifier] -> (values goal? immutable-free-id-set?))
(define (remove-unused-vars g vars-in-scope)
  (syntax-parse g
    #:literal-sets (mk-literals)
    [c:primitive-goal (values this-syntax (immutable-free-id-set))]
    [(c:unary-constraint t) (values this-syntax (term-refs #'t vars-in-scope))]
    [(c:binary-constraint t1 t2)
     (values this-syntax (free-id-set-union (term-refs #'t1 vars-in-scope)
                                            (term-refs #'t2 vars-in-scope)))]
    [(disj g1 g2)
     (let-values ([(g1^ g1-refs) (remove-unused-vars #'g1 vars-in-scope)]
                  [(g2^ g2-refs) (remove-unused-vars #'g2 vars-in-scope)])
       (values #`(disj #,g1^ #,g2^) (free-id-set-union g1-refs g2-refs)))]
    [(conj g1 g2)
     (let-values ([(g1^ g1-refs) (remove-unused-vars #'g1 vars-in-scope)]
                  [(g2^ g2-refs) (remove-unused-vars #'g2 vars-in-scope)])
       (values #`(conj #,g1^ #,g2^) (free-id-set-union g1-refs g2-refs)))]
    [(fresh (x ...) g)
     (let-values ([(g^ g-refs) (remove-unused-vars #'g (append (attribute x) vars-in-scope))])
       (define vars-to-keep (filter (λ (lv) (free-id-set-member? g-refs lv)) (syntax->list #'(x ...))))
       (define free-refs (free-id-set-subtract g-refs (immutable-free-id-set vars-to-keep)))
       (values #`(fresh (#,@vars-to-keep) #,g^) free-refs))]
    [(#%rel-app n t ...)
     (values this-syntax
             (for/fold ([var-refs (immutable-free-id-set)])
                       ([t (in-syntax #'(t ...))])
               (free-id-set-union var-refs (term-refs t vars-in-scope))))]
    [(goal-from-expression e)
     (values (syntax-property this-syntax TERM-VARS-IN-SCOPE (map flip-intro-scope vars-in-scope)) (immutable-free-id-set vars-in-scope))]
    [(apply-relation e t ...)
     (values this-syntax
             (for/fold ([var-refs (immutable-free-id-set)])
                       ([t (in-syntax #'(t ...))])
               (free-id-set-union var-refs (term-refs t vars-in-scope))))]))

(define (term-refs t vars-in-scope)
  (syntax-parse t #:literal-sets (mk-literals) #:literals (cons quote)
    [(#%lv-ref v)
     (immutable-free-id-set (list #'v))]
    [(term-from-expression _) (immutable-free-id-set vars-in-scope)]
    [(quote _) (immutable-free-id-set)]
    [(cons t1 t2)
     (free-id-set-union (term-refs #'t1 vars-in-scope) (term-refs #'t2 vars-in-scope))]))

(module* test racket/base
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           (except-in rackunit fail)
           (for-syntax racket/base
                       syntax/parse
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  (begin-for-syntax
    (define (remove-unused-vars/rel stx)
      (syntax-parse stx
        [(ir-rel (x ...) g)
         #`(ir-rel (x ...) #,(remove-unused-vars/entry #'g (attribute x) #f))])))

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

;; When there's a goal from expression; we must assume that every
;; variable is refrered to from within that expression.
  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ((~binders a))
          (fresh ((~binders x y))
            (conj
              ;; This isn't a valid program ofc; just compiler pass test
              (goal-from-expression #t)
              (== (#%lv-ref x) (#%lv-ref a)))))))
    (generate-prog
      (ir-rel ((~binders a))
        (fresh ((~binders x y))
          (conj
            ;; This isn't a valid program ofc; just compiler pass test
            (goal-from-expression #t)
            (== (#%lv-ref x) (#%lv-ref a)))))))

  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ()
          (fresh ((~binder a))
            (== (#%lv-ref a) (quote 5))))))
    (generate-prog
      (ir-rel ()
        (fresh ((~binder a))
          (== (#%lv-ref a) (quote 5))))))

  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ()
          (disj
            (fresh ((~binder x))
              (== (#%lv-ref x) (#%lv-ref x)))
            (fresh ((~binder y))
              (== (quote 5) (quote 6)))))))

    (generate-prog
      (ir-rel ()
        (disj
          (fresh ((~binder x))
            (== (#%lv-ref x) (#%lv-ref x)))
          (fresh ()
            (== (quote 5) (quote 6)))))))

  (progs-equal?
    (remove-unused-vars/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (fresh ((~binder y))
              (conj
                (== (#%lv-ref x) (quote 5))
                (fresh ((~binder z))
                  (== (#%lv-ref z) (#%lv-ref y)))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
              (fresh ((~binder y))
                (conj
                  (== (#%lv-ref x) (quote 5))
                  (fresh ((~binder z))
                    (== (#%lv-ref z) (#%lv-ref y)))))))))

  )
