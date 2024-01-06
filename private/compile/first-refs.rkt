#lang racket/base

(require syntax/parse
         syntax/id-set
         (only-in racket/sequence in-syntax)
         (for-template racket/base
                       "../forms.rkt")
         "prop-vars.rkt"
         "../syntax-classes.rkt")

(provide first-refs/entry)

(define (first-refs/entry g fvs fvs-fresh?)
  (let*-values ([(id-refs) (if fvs-fresh?
                             (immutable-free-id-set fvs)
                             (immutable-free-id-set))]
                [(g^ _) (annotate-goal g id-refs)])
    g^))

;; Takes a goal and the vars to which we have not referred, and returns
;; an annotated goal and the new updated set of vars to which we have still not referred after
;;
;; Goal FIdSet -> (values Goal FIdSet)
(define (annotate-goal g id-refs)
  (syntax-parse g #:literal-sets (mk-literals)
    [c:primitive-goal (values this-syntax id-refs)]
    [(c:unary-constraint t)
     (let-values ([(t^ refs^) (annotate-term #'t id-refs)])
       (values #`(c #,t^) refs^))]
    [(== t1 t2)
     (let*-values ([(t1^ refs^) (annotate-term #'t1 id-refs)]
                   [(t2^ refs^^) (annotate-term #'t2 refs^)]
                   [(stx^) #`(== #,t1^ #,t2^)])
       (if (syntax-property g SKIP-CHECK)
           (values (set-skip-check stx^) refs^^)
           (values stx^ refs^^)))]
    [(c:binary-constraint t1 t2)
     (let*-values ([(t1^ refs^) (annotate-term #'t1 id-refs)]
                   [(t2^ refs^^) (annotate-term #'t2 refs^)])
       (values #`(c #,t1^ #,t2^) refs^^))]
    [(conj g1 g2)
     (let*-values ([(g1^ refs^) (annotate-goal #'g1 id-refs)]
                   [(g2^ refs^^) (annotate-goal #'g2 refs^)])
       (values #`(conj #,g1^ #,g2^) refs^^))]
    [(disj g1 g2)
     (let-values ([(g1^ fst-refs) (annotate-goal #'g1 id-refs)]
                  [(g2^ snd-refs) (annotate-goal #'g2 id-refs)])
       (values #`(disj #,g1^ #,g2^) (free-id-set-intersect fst-refs snd-refs)))]
    [(fresh (x ...) g)
     (let-values ([(g^ refs^) (annotate-goal #'g (free-id-set-union id-refs (immutable-free-id-set (attribute x))))])
       (values #`(fresh (x ...) #,g^) refs^))]
    [(#%rel-app n t ...)
     (define-values (rev-ts refs^)
       (for/fold ([done '()]
                  [refs id-refs])
                 ([t (in-syntax #'(t ...))])
         (let-values ([(t^ refs^) (annotate-term t refs)])
           (values (cons t^ done) refs^))))
     (values #`(#%rel-app n #,@(reverse rev-ts)) refs^)]
    [(goal-from-expression e)
     (values this-syntax (immutable-free-id-set))]
    [(apply-relation e t ...)
     (define-values (rev-ts refs^)
       (for/fold ([done '()]
                  [refs id-refs])
                 ([t (in-syntax #'(t ...))])
         (let-values ([(t^ refs^) (annotate-term t refs)])
           (values (cons t^ done) refs^))))
     (values #`(apply-relation e #,@(reverse rev-ts)) refs^)]))

;; Given a mK term and the set of logic variable to which we have not
;; referred yet, return an updated term (possibly annotated
;; w/FIRST-REF), and an updated set of variables, removing the ones we
;; first refer to in /THIS/ term.
;;
;; Term FIdSet -> (Values Term FIdSet)
(define (annotate-term t id-refs)
  (syntax-parse t
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(quote d) (values this-syntax id-refs)]
    [(term-from-expression e) (values this-syntax (immutable-free-id-set))]
    [(#%lv-ref v)
     (if (free-id-set-member? id-refs #'v)
       (values (set-first-ref this-syntax) (free-id-set-remove id-refs #'v))
       (values this-syntax id-refs))]
    [(cons t1 t2)
     (let*-values ([(t1^ refs^) (annotate-term #'t1 id-refs)]
                   [(t2^ refs^^) (annotate-term #'t2 refs^)])
       (values #`(cons #,t1^ #,t2^) refs^^))]))

(module+ test
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           (except-in rackunit fail)
           (for-syntax racket/base
                       syntax/parse
                       "./test/unit-test-progs.rkt"
                       "prop-vars.rkt"
                       (submod "..")))

(begin-for-syntax
  (define (first-refs/rel stx)
    (syntax-parse stx #:literal-sets (mk-literals)
      [(ir-rel (x ...) g)
       #`(ir-rel (x ...) #,(first-refs/entry #'g (attribute x) #f))])))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (#%lv-ref q) (quote 5)))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (~missing (#%lv-ref q) FIRST-REF) (quote 5)))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (== (#%lv-ref q) (#%lv-ref x))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (== (~missing (#%lv-ref q) FIRST-REF) (~check (#%lv-ref x) FIRST-REF))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
             (goal-from-expression #t)
             (== (#%lv-ref q) (#%lv-ref x)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (conj
           (goal-from-expression #t)
           (== (~missing (#%lv-ref q) FIRST-REF) (~missing (#%lv-ref x) FIRST-REF)))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (== (#%lv-ref x) (quote 5))
              (== (#%lv-ref q) (#%lv-ref x)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (conj
            (== (~check (#%lv-ref x) FIRST-REF) (quote 5))
            (== (~missing (#%lv-ref q) FIRST-REF) (~missing (#%lv-ref x) FIRST-REF)))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (== (#%lv-ref q) (cons (#%lv-ref x) (#%lv-ref x)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (== (#%lv-ref q)
              (cons (~check (#%lv-ref x) FIRST-REF)
                    (~missing (#%lv-ref x) FIRST-REF)))))))

  (let ([foo 5])
    (progs-equal?
      (first-refs/rel
        (generate-prog
          (ir-rel ((~binder q))
            (fresh ((~binder x))
              (conj
                (#%rel-app foo (#%lv-ref x))
                (== (#%lv-ref q) (#%lv-ref x)))))))
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (#%rel-app foo (~check (#%lv-ref x) FIRST-REF))
              (== (#%lv-ref q) (~missing (#%lv-ref x) FIRST-REF))))))))


  (let ([foo 5])
    (progs-equal?
      (first-refs/rel
        (generate-prog
          (ir-rel ((~binder q))
            (fresh ((~binder x))
              (conj
                (apply-relation foo (#%lv-ref x))
                (== (#%lv-ref q) (#%lv-ref x)))))))
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (apply-relation foo (~check (#%lv-ref x) FIRST-REF))
              (== (#%lv-ref q) (~missing (#%lv-ref x) FIRST-REF))))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (disj
              (== (#%lv-ref x) (quote 5))
              (== (#%lv-ref x) (quote 6)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
         (disj
           (== (~check (#%lv-ref x) FIRST-REF) (quote 5))
           (== (~check (#%lv-ref x) FIRST-REF) (quote 6)))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (disj
                (== (#%lv-ref x) (quote 5))
                (== (#%lv-ref y) (quote 5)))
              (== (#%lv-ref q) (cons (#%lv-ref x) (#%lv-ref y))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (disj
              (== (~check (#%lv-ref x) FIRST-REF) (quote 5))
              (== (~check (#%lv-ref y) FIRST-REF) (quote 5)))
            (== (#%lv-ref q)
                (cons (~missing (#%lv-ref x) FIRST-REF)
                      (~missing (#%lv-ref y) FIRST-REF))))))))

  ;; SKIP-CHECK is preserved and FIRST-REF is added
  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (~prop (== (#%lv-ref x) (quote 5)) SKIP-CHECK #t)))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (~check (== (#%lv-ref x) (quote 5)) SKIP-CHECK)))))
  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (~prop (== (#%lv-ref x) (quote 5)) SKIP-CHECK #t)))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (== (~check (#%lv-ref x) FIRST-REF) (quote 5))))))

  )
