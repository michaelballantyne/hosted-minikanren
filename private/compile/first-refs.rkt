#lang racket/base

(require syntax/parse
         syntax/id-set
         (only-in racket/sequence in-syntax)
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt")

(provide first-refs/rel
         first-refs/run)

(define (first-refs/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     (let-values ([(g^ _) (annotate-goal #'g (immutable-free-id-set (syntax->list #'(x ...))))])
       #`(ir-rel (x ...) #,g^))]))

(define (first-refs/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let-values ([(g^ _) (annotate-goal #'g (immutable-free-id-set))])
       #`(run n (q ...) #,g^))]
    [(run* (q ...) g)
     (let-values ([(g^ _) (annotate-goal #'g (immutable-free-id-set))])
       #`(run* (q ...) #,g^))]))

(define (annotate-goal g id-refs)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:unary-constraint t)
     (let-values ([(t^ refs^) (annotate-term #'t id-refs)])
       (values #`(c #,t^) refs^))]
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
       (values #`(disj #,g1^ #,g2^) (free-id-set-union fst-refs snd-refs)))]
    [(fresh (x ...) g)
     (let-values ([(g^ refs^) (annotate-goal #'g id-refs)])
       (values #`(fresh (x ...) #,g^) refs^))]
    [(#%rel-app n t ...)
     (define-values (rev-ts refs^)
       (for/fold ([done '()]
                  [refs id-refs])
                 ([t (in-syntax #'(t ...))])
         (let-values ([(t^ refs^) (annotate-term t refs)])
           (values (cons t^ done) refs^))))
     (values #`(#%rel-app n #,@(reverse rev-ts)) refs^)]
    [(apply-relation e t ...)
     (define-values (rev-ts refs^)
       (for/fold ([done '()]
                  [refs id-refs])
                 ([t (in-syntax #'(t ...))])
         (let-values ([(t^ refs^) (annotate-term t refs)])
           (values (cons t^ done) refs^))))
     (values #`(apply-relation e #,@(reverse rev-ts)) refs^)]))

(define (annotate-term t id-refs)
  (syntax-parse t
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%term-datum l) (values this-syntax id-refs)]
    [(quote d) (values this-syntax id-refs)]
    [(rkt-term e) (values this-syntax id-refs)]
    [(#%lv-ref v)
     (if (free-id-set-member? id-refs #'v)
       (values this-syntax id-refs)
       (values (syntax-property this-syntax 'first-ref #t) (free-id-set-add id-refs #'v)))]
    [(cons t1 t2)
     (let*-values ([(t1^ refs^) (annotate-term #'t1 id-refs)]
                   [(t2^ refs^^) (annotate-term #'t2 refs^)])
       (values #`(cons #,t1^ #,t2^) refs^^))]))

(module+ test
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (#%lv-ref q) (#%term-datum 5)))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (~missing (#%lv-ref q) 'first-ref) (#%term-datum 5)))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (== (#%lv-ref q) (#%lv-ref x))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
              (== (~missing (#%lv-ref q) 'first-ref) (~check (#%lv-ref x) 'first-ref))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (== (#%lv-ref x) (#%term-datum 5))
              (== (#%lv-ref q) (#%lv-ref x)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (conj
            (== (~check (#%lv-ref x) 'first-ref) (#%term-datum 5))
            (== (~missing (#%lv-ref q) 'first-ref) (~missing (#%lv-ref x) 'first-ref)))))))

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
              (cons (~check (#%lv-ref x) 'first-ref)
                    (~missing (#%lv-ref x) 'first-ref)))))))

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
              (#%rel-app foo (~check (#%lv-ref x) 'first-ref))
              (== (#%lv-ref q) (~missing (#%lv-ref x) 'first-ref))))))))


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
              (apply-relation foo (~check (#%lv-ref x) 'first-ref))
              (== (#%lv-ref q) (~missing (#%lv-ref x) 'first-ref))))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (disj
              (== (#%lv-ref x) (#%term-datum 5))
              (== (#%lv-ref x) (#%term-datum 6)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
         (disj
           (== (~check (#%lv-ref x) 'first-ref) (#%term-datum 5))
           (== (~check (#%lv-ref x) 'first-ref) (#%term-datum 6)))))))

  (progs-equal?
    (first-refs/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (disj
                (== (#%lv-ref x) (#%term-datum 5))
                (== (#%lv-ref y) (#%term-datum 5)))
              (== (#%lv-ref q) (cons (#%lv-ref x) (#%lv-ref y))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (disj
              (== (~check (#%lv-ref x) 'first-ref) (#%term-datum 5))
              (== (~check (#%lv-ref y) 'first-ref) (#%term-datum 5)))
            (== (#%lv-ref q)
                (cons (~missing (#%lv-ref x) 'first-ref)
                      (~missing (#%lv-ref y) 'first-ref))))))))


  )
