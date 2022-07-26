#lang racket/base

(require syntax/parse
         syntax/id-table
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt")

(provide fold/rel
         fold/run)


(define (empty-subst) (make-immutable-free-id-table))
(define (ext-subst u v s) (free-id-table-set s u v))

(define (walk t s)
  (let rec ([t t])
    (if (identifier? t)
      (let ([val (free-id-table-ref s t #f)])
        (if val (rec val) t))
      t)))

(define (equal-vals? u v)
  (syntax-parse (list u v)
    #:literal-sets (mk-literals)
    #:literals (cons quote)
    [((#%term-datum l1) (#%term-datum l2))
     (equal? (syntax->datum #'l1) (syntax->datum #'l2))]
    [((quote v1) (quote v2)) (equal? (syntax->datum #'v1) (syntax->datum #'v2))]
    [((#%lv-ref v1:id) (#%lv-ref v2:id))
     (free-identifier=? #'v1 #'v2)]
    [((cons a1 d1) (cons a2 d2))
     (and (equal-vals? #'a1 #'a2)
          (equal-vals? #'d1 #'d2))]
    [_ #f]))

(define (unify u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (syntax-parse (list u v)
      #:literal-sets (mk-literals)
      #:literals (cons quote)
      [_ #:when (equal-vals? u v) (values #'(success) s)]
      [((#%lv-ref id1:id) (#%lv-ref id2:id))
       (values #`(== (#%lv-ref id1) (#%lv-ref id2)) (ext-subst #'id1 #'id2 s))]
      [((#%lv-ref id1:id) _)
       (values #`(== (#%lv-ref id1) #,v) (ext-subst #'id1 v s))]
      [(_ (#%lv-ref id2:id))
       (values #`(== (#%lv-ref id2) #,u) (ext-subst #'id2 u s))]
      [((cons a1 d1) (cons a2 d2))
       (let*-values ([(g1^ s^) (unify #'a1 #'a2 s)]
                     [(g2^ s^^) (unify #'d1 #'d2 s^)])
         (values #`(conj #,g1^ #,g2^) s^^))]
      [((rkt-term _) _) (values #`(== #,u #,v) s)]
      [(_ (rkt-term _)) (values #`(== #,v #,u) s)]
      [_ (values #'(failure) s)])))

(define (fold/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     (let-values ([(new-g _) (fold/goal #'g (empty-subst))])
       #`(ir-rel (x ...) #,new-g))]))

(define (fold/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let-values ([(new-g _) (fold/goal #'g (empty-subst))])
       #`(run n (q ...) #,new-g))]
    [(run* (q ...) g)
     (let-values ([(new-g _) (fold/goal #'g (empty-subst))])
       #`(run* (q ...) #,new-g))]))

;; INVARIANT: goals cannot be removed, only added (by inserting conjunctions where there were previously flat goals).
;; no-ops/successes will be removed by a future pass.
(define (fold/goal g subst)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:unary-constraint t) (values this-syntax subst)]
    [(== t1 t2) (unify #'t1 #'t2 subst)]
    [(c:binary-constraint t1 t2) (values this-syntax subst)]
    [(conj g1 g2)
     (let*-values ([(g1^ s^) (fold/goal #'g1 subst)]
                   [(g2^ s^^) (fold/goal #'g2 s^)])
       (values #`(conj #,g1^ #,g2^) s^^))]
    ;; IDEA: basically assume we don't gain any information from disjunction because it could be self-contradictory
    [(disj g1 g2)
     (let-values ([(g1^ _s1) (fold/goal #'g1 subst)]
                  [(g2^ _s2) (fold/goal #'g2 subst)])
       (values #`(disj #,g1^ #,g2^) subst))]
    [(fresh (x ...) g)
     (let-values ([(g^ s^) (fold/goal #'g subst)])
       (values #`(fresh (x ...) #,g^) s^))]
    [(#%rel-app n t ...) (values this-syntax subst)]
    [(apply-relation e t ...) (values this-syntax subst)]))

(module+ test
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (#%term-datum 5) (#%lv-ref q)))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (#%lv-ref q) (#%term-datum 5)))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (== (#%lv-ref q) (#%lv-ref x))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (== (#%lv-ref q) (#%lv-ref x))))))
  
  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (cons (#%term-datum 5) (#%lv-ref q))
              (cons (#%term-datum 5) (#%term-datum 5))))))
    (generate-prog
      (ir-rel ((~binder q))
        (conj
          (success)
          (== (#%lv-ref q) (#%term-datum 5))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (cons (#%term-datum 5) (#%lv-ref q))
              (cons (#%term-datum 6) (#%lv-ref q))))))
      (generate-prog
        (ir-rel ((~binder q))
          (conj
            (failure)
            (success)))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (== (cons (#%lv-ref q) (#%term-datum 5))
                (cons (#%lv-ref x) (#%lv-ref y)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (== (#%lv-ref q) (#%lv-ref x))
            (== (#%lv-ref y) (#%term-datum 5)))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (== (cons (#%lv-ref q) (cons (#%lv-ref x) (cons (#%lv-ref y) '())))
                (cons (quote (~dat-lit cat))
                      (cons (quote (~dat-lit dog))
                            (cons (quote (~dat-lit mouse)) '()))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (== (#%lv-ref q) (quote (~dat-lit cat)))
            (conj
              (== (#%lv-ref x) (quote (~dat-lit dog)))
              (conj
                (== (#%lv-ref y) (quote (~dat-lit mouse)))
                (success))))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ()
          (== (#%term-datum 5) (#%term-datum 5)))))
    (generate-prog
      (ir-rel ()
        (success))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ()
          (== (#%term-datum 5) (#%term-datum 6)))))
    (generate-prog
      (ir-rel ()
        (failure))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ()
          (== (rkt-term 5) (rkt-term 6)))))
    (generate-prog
      (ir-rel ()
        (== (rkt-term 5) (rkt-term 6)))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (cons (#%term-datum 5) (#%lv-ref q))
              (cons (#%term-datum 5) (#%lv-ref q))))))
    (generate-prog
      (ir-rel ((~binder q))
        (success))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (== (cons (cons (#%term-datum 5) (#%term-datum 6))
                    (#%lv-ref q))
              (cons (cons (#%term-datum 5) (#%term-datum 6))
                    (#%lv-ref p))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (conj
          (success)
          (== (#%lv-ref q) (#%lv-ref p))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ()
          (conj
            (== (#%term-datum 5) (rkt-term 5))
            (conj
              (== (quote (~dat-lit 3)) (rkt-term 3))
              (== (cons (#%term-datum 3) (#%term-datum 4))
                  (rkt-term (cons 3 4))))))))
    (generate-prog
      (ir-rel ()
        (conj
          (== (rkt-term 5) (#%term-datum 5))
          (conj
            (== (rkt-term 3) (quote (~dat-lit 3)))
            (== (rkt-term (cons 3 4))
                (cons (#%term-datum 3) (#%term-datum 4))))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (cons (#%term-datum 1) (#%lv-ref q))
              ;; '(1 2)))))
              (cons (#%term-datum 1) (#%term-datum 2))))))
    (generate-prog
      (ir-rel ((~binder q))
        (conj
          (success)
          (== (#%lv-ref q) (#%term-datum 2))))))

  

        
    )
