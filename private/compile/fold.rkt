#lang racket/base

(require syntax/parse
         syntax/id-table
         syntax/stx
         racket/function
         racket/match
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt")

(provide fold/rel
         fold/run)


(define (empty-subst) (make-immutable-free-id-table))
(define (ext-subst u v s) (free-id-table-set s u v))

(define (empty-var-debruijn-level) (make-immutable-free-id-table))
(define init-next-var-level-counter 0)
(define (add-var-debruijn-level u v s) (free-id-table-set s u v))
(define (var-debruijn-level-get s v) (free-id-table-ref s v #f))

(define (walk t s)
  (let rec ([t t])
    (syntax-parse t
      #:literal-sets (mk-literals)
      [(#%lv-ref v)
       (let ([val (free-id-table-ref s #'v #f)])
         (if val (rec val) t))]
      [_ t])))

(define (walk-to-last-var t s)
  (let rec ([t t])
    (syntax-parse t
      #:literal-sets (mk-literals)
      [(#%lv-ref v)
       (let ([val (free-id-table-ref s #'v #f)])
         (if val
             (syntax-parse val
               #:literal-sets (mk-literals)
               [(#%lv-ref v2)
                (rec val)]
               [_ t])
             t))]
      [_ t])))

(define (maybe-inline t t^ s)
  (syntax-parse t^
    #:literal-sets (mk-literals)
    #:literals (cons quote)
    [(quote vl)
     t^]
    [(#%lv-ref v)
     t^]
    [(cons t1 t2)
     (walk-to-last-var t s)]
    [(rkt-term _)
     (walk-to-last-var t s)]))

(define (equal-vals? u v)
  (syntax-parse (list u v)
    #:literal-sets (mk-literals)
    #:literals (cons quote)
    [((quote v1) (quote v2)) (equal? (syntax->datum #'v1) (syntax->datum #'v2))]
    [((#%lv-ref v1:id) (#%lv-ref v2:id))
     (free-identifier=? #'v1 #'v2)]
    [((cons a1 d1) (cons a2 d2))
     (and (equal-vals? #'a1 #'a2)
          (equal-vals? #'d1 #'d2))]
    [_ #f]))

(define (db<= p1 p2)
  (or (<= (car p1) (car p2))
      (and (= (car p1) (car p2))
           (< (cdr p1) (cdr p2)))))

(define (unify u v s dict)
  (let ([u^ (walk u s)]
        [v^ (walk v s)])
    (syntax-parse (list u^ v^)
      #:literal-sets (mk-literals)
      #:literals (cons quote)
      [_ #:when (equal-vals? u^ v^) (values #'(success) s)]
      [((#%lv-ref id1:id) (#%lv-ref id2:id))
       (let ([dl1 (var-debruijn-level-get dict #'id1)]
             [dl2 (var-debruijn-level-get dict #'id2)])
         (if (db<= dl1 dl2)
             (values #`(== (#%lv-ref id2) #,(maybe-inline u u^ s)) (ext-subst #'id2 u s))
             (values #`(== (#%lv-ref id1) #,(maybe-inline v v^ s)) (ext-subst #'id1 v s))))]
      [((#%lv-ref id1:id) _)
       (values #`(== (#%lv-ref id1) #,(maybe-inline v v^ s)) (ext-subst #'id1 v s))]
      [(_ (#%lv-ref id2:id))
       (values #`(== (#%lv-ref id2) #,(maybe-inline u u^ s)) (ext-subst #'id2 u s))]
      [((cons a1 d1) (cons a2 d2))
       (let*-values ([(g1^ s^) (unify #'a1 #'a2 s dict)]
                     [(g2^ s^^) (unify #'d1 #'d2 s^ dict)])
         (values #`(conj #,g1^ #,g2^) s^^))]
      [((rkt-term _) _) (values #`(== #,u^ #,v^) s)]
      [(_ (rkt-term _)) (values #`(== #,v^ #,u^) s)]
      [_ (values #'(failure) s)])))

(define (map-maybe-inline* subst stx)
  (stx-map (curryr maybe-inline subst) stx (stx-map (curryr walk subst) stx)))

(define (compute-levels-for dict next lov)
  (for/fold ([dict dict])
            ([v (in-list lov)]
             [i (in-naturals)])
    (add-var-debruijn-level v (cons next i) dict)))

(define (add-first-level lov)
  (add-level-to (level-dict (empty-var-debruijn-level) init-next-var-level-counter) lov))

(struct level-dict (dict next))

(define (add-level-to old-ld lov)
  (match-define (level-dict dict next) old-ld)
  (level-dict (compute-levels-for dict next lov) (add1 next)))

(define (fold/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     (let-values ([(new-g _) (fold/goal #'g (empty-subst) (add-first-level (attribute x)))])
       #`(ir-rel (x ...) #,new-g))]))

(define (fold/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let-values ([(new-g _) (fold/goal #'g (empty-subst) (add-first-level (attribute q)))])
       #`(run n (q ...) #,new-g))]
    [(run* (q ...) g)
     (let-values ([(new-g _) (fold/goal #'g (empty-subst) (add-first-level (attribute q)))])
       #`(run* (q ...) #,new-g))]))

;; INVARIANT: goals cannot be removed, only added (by inserting conjunctions where there were previously flat goals).
;; no-ops/successes will be removed by a future pass.
(define (fold/goal g subst ld)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:unary-constraint t)
     (values #`(c . #,(map-maybe-inline* subst #'(t))) subst)]
    [(== t1 t2) (unify #'t1 #'t2 subst (level-dict-dict ld))]
    [(c:binary-constraint t1 t2)
     (values #`(c . #,(map-maybe-inline* subst #'(t1 t2))) subst)]
    [(conj g1 g2)
     (let*-values ([(g1^ s^) (fold/goal #'g1 subst ld)]
                   [(g2^ s^^) (fold/goal #'g2 s^ ld)])
       (values #`(conj #,g1^ #,g2^) s^^))]
    ;; IDEA: basically assume we don't gain any information from disjunction because it could be self-contradictory
    [(disj g1 g2)
     (let-values ([(g1^ _s1) (fold/goal #'g1 subst ld)]
                  [(g2^ _s2) (fold/goal #'g2 subst ld)])
       (values #`(disj #,g1^ #,g2^) subst))]
    ;; Assume we don't gain any information within a fresh because we can't inline terms that may
    ;; refer to a variable in the fresh into contexts outside of the fresh. This is an unfortunate
    ;; limitation and should be alleviated by a better design.
    [(fresh (x ...) g)
     (let-values ([(g^ s^) (fold/goal #'g subst (add-level-to ld (attribute x)))])
       (values #`(fresh (x ...) #,g^) subst))]
    [(#%rel-app n t ...)
     (values #`(#%rel-app n . #,(map-maybe-inline* subst #'(t ...))) subst)]
    [(apply-relation e t ...)
     (values #`(apply-relation e . #,(map-maybe-inline* subst #'(t ...))) subst)]))

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
          (== (quote 5) (#%lv-ref q)))))
    (generate-prog
      (ir-rel ((~binder q))
        (== (#%lv-ref q) (quote 5)))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (== (#%lv-ref x) (#%lv-ref q))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (== (#%lv-ref x) (#%lv-ref q))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (cons (quote 5) (#%lv-ref q))
              (cons (quote 5) (quote 5))))))
    (generate-prog
      (ir-rel ((~binder q))
        (conj
          (success)
          (== (#%lv-ref q) (quote 5))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (cons (quote 5) (#%lv-ref q))
              (cons (quote 6) (#%lv-ref q))))))
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
            (== (cons (#%lv-ref q) (quote 5))
                (cons (#%lv-ref x) (#%lv-ref y)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (== (#%lv-ref x) (#%lv-ref q))
            (== (#%lv-ref y) (quote 5)))))))

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
     (ir-rel ((~binder result1))
        (== (cons (quote (~dat-lit cat)) (quote (~dat-lit fish)))
            (cons (quote (~dat-lit cat)) (#%lv-ref result1))))))
    (generate-prog
     (ir-rel ((~binder result1))
        (conj
          (success)
          (== (#%lv-ref result1)
              (quote (~dat-lit fish)))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ()
          (== (quote 5) (quote 5)))))
    (generate-prog
      (ir-rel ()
        (success))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ()
          (== (quote 5) (quote 6)))))
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
          (== (cons (quote 5) (#%lv-ref q))
              (cons (quote 5) (#%lv-ref q))))))
    (generate-prog
      (ir-rel ((~binder q))
        (success))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (== (cons (cons (quote 5) (quote 6))
                    (#%lv-ref q))
              (cons (cons (quote 5) (quote 6))
                    (#%lv-ref p))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (conj
          (success)
          (== (#%lv-ref p) (#%lv-ref q))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ()
          (conj
            (== (quote 5) (rkt-term 5))
            (conj
              (== (quote (~dat-lit 3)) (rkt-term 3))
              (== (cons (quote 3) (quote 4))
                  (rkt-term (cons 3 4))))))))
    (generate-prog
      (ir-rel ()
        (conj
          (== (rkt-term 5) (quote 5))
          (conj
            (== (rkt-term 3) (quote (~dat-lit 3)))
            (== (rkt-term (cons 3 4))
                (cons (quote 3) (quote 4))))))))

  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (cons (quote 1) (#%lv-ref q))
              (cons (quote 1) (quote 2))))))
    (generate-prog
      (ir-rel ((~binder q))
        (conj
          (success)
          (== (#%lv-ref q) (quote 2))))))

  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
             (fresh ((~binders a b c))
               (conj
                (conj
                 (== (#%lv-ref a) (cons (quote 1) (cons (quote 2) (quote ()))))
                 (== (#%lv-ref b) (#%lv-ref a)))
                 (== (#%lv-ref c) (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
            (fresh ((~binders a b c))
              (conj
                (conj
                 (== (#%lv-ref a) (cons (quote 1) (cons (quote 2) (quote ()))))
                 (== (#%lv-ref b) (#%lv-ref a)))
                 (== (#%lv-ref c) (#%lv-ref a)))))))

  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
             (fresh ((~binders a b c))
               (conj
                (conj
                 (== (#%lv-ref a) (rkt-term '(1 2 3)))
                 (== (#%lv-ref b) (#%lv-ref a)))
                 (== (#%lv-ref c) (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
            (fresh ((~binders a b c))
              (conj
                (conj
                 (== (#%lv-ref a) (rkt-term '(1 2 3)))
                 (== (#%lv-ref b) (#%lv-ref a)))
                 (== (#%lv-ref c) (#%lv-ref a)))))))

  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
             (fresh ((~binders a b))
               (conj
                (== (#%lv-ref a) (cons (quote 1) (cons (quote 2) (quote ()))))
                (== (#%lv-ref b) (cons (#%lv-ref a) (#%lv-ref a))))))))
   (generate-prog
     (ir-rel ()
             (fresh ((~binders a b))
               (conj
                (== (#%lv-ref a) (cons (quote 1) (cons (quote 2) (quote ()))))
                (== (#%lv-ref b) (cons (#%lv-ref a) (#%lv-ref a))))))))

  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
             (fresh ((~binders a b c))
               (conj
                (conj
                 (== (#%lv-ref a) (quote 1))
                 (== (#%lv-ref b) (#%lv-ref a)))
                 (== (#%lv-ref c) (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
            (fresh ((~binders a b c))
              (conj
                (conj
                 (== (#%lv-ref a) (quote 1))
                 (== (#%lv-ref b) (quote 1)))
                 (== (#%lv-ref c) (quote 1)))))))

  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ((~binder x))
             (fresh ((~binder l))
               (conj
                (== (#%lv-ref l) (cons (#%lv-ref x) (quote ())))
                (disj
                 (fresh ((~binder y))
                   (== (#%lv-ref l) (cons (#%lv-ref y) (quote ()))))
                 (== (#%lv-ref l) (cons (quote 1) (quote ())))))))))
   (generate-prog
    (ir-rel ((~binder x))
            (fresh ((~binder l))
              (conj
               (== (#%lv-ref l) (cons (#%lv-ref x) (quote ())))
               (disj
                (fresh ((~binder y))
                  (conj
                   (== (#%lv-ref y) (#%lv-ref x))
                   (success)))
                (conj
                 (== (#%lv-ref x) (quote 1))
                 (success))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ((~binder a))
             (fresh ((~binders b))
               (conj
                (== (#%lv-ref b) (#%lv-ref a))
                (#%rel-app foo (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ((~binder a))
            (fresh ((~binders b))
              (conj
                (== (#%lv-ref b) (#%lv-ref a))
                (#%rel-app foo (#%lv-ref a))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ((~binder a))
             (fresh ((~binders b))
               (conj
                (== (#%lv-ref a) (#%lv-ref b))
                (#%rel-app foo (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ((~binder a))
            (fresh ((~binders b))
              (conj
                (== (#%lv-ref b) (#%lv-ref a))
                (#%rel-app foo (#%lv-ref a))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
        (fresh ((~binders b))
          (conj
           (== (#%lv-ref b) (quote ()))
           (#%rel-app foo (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
       (fresh ((~binders b))
         (conj
           (== (#%lv-ref b) (quote ()))
           (#%rel-app foo (quote ()))))))))


(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
        (fresh ((~binders b))
          (conj
           (== (quote 5) (#%lv-ref b))
           (#%rel-app foo (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
       (fresh ((~binders b))
         (conj
           (== (#%lv-ref b) (quote 5))
           (#%rel-app foo (quote 5))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
        (fresh ((~binders b))
          (conj
           (== (quote ()) (#%lv-ref b))
           (#%rel-app foo (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
       (fresh ((~binders b))
         (conj
           (== (#%lv-ref b) (quote ()))
           (#%rel-app foo (quote ()))))))))


(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ((~binder a))
       (fresh ()
         (conj
           (== (#%lv-ref a) (quote ()))
           (#%rel-app foo (#%lv-ref a)))))))
   (generate-prog
    [ir-rel ((~binder a))
      (fresh ()
        (conj
          (== (#%lv-ref a) (quote ()))
          (#%rel-app foo (quote ()))))])))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ((~binder a))
       (fresh ()
         (conj
           (== (#%lv-ref a) (quote 1))
           (#%rel-app foo (#%lv-ref a)))))))
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ()
        (conj
          (== (#%lv-ref a) (quote 1))
          (#%rel-app foo (quote 1))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
        (fresh ((~binders b))
          (conj
           (== (cons (quote 1) (quote ())) (#%lv-ref b))
           (#%rel-app foo (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
       (fresh ((~binders b))
         (conj
           (== (#%lv-ref b) (cons (quote 1) (quote ())))
           (#%rel-app foo (#%lv-ref b))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
        (fresh ((~binders b))
          (conj
           (== (quote 1) (#%lv-ref b))
           (=/= (quote ()) (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
       (fresh ((~binders b))
         (conj
          (== (#%lv-ref b) (quote 1))
          (=/= (quote ()) (quote 1))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ()
        (fresh ((~binders b))
          (conj
           (== (quote 1) (#%lv-ref b))
           (symbolo (#%lv-ref b)))))))
   (generate-prog
    (ir-rel ()
       (fresh ((~binders b))
         (conj
          (== (#%lv-ref b) (quote 1))
          (symbolo (quote 1))))))))


  ;; This test documents a limitation: we don't propagate information out of a fresh,
  ;; so as to avoid inlining out-of-context references. We should be able to do better.
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ((~binder a))
             (conj
              (fresh ((~binder b))
                (== (#%lv-ref a) (cons (#%lv-ref b) (quote ()))))
              (== (#%lv-ref a) (cons (quote 1) (quote ())))))))
   (generate-prog
    (ir-rel ((~binder a))
             (conj
              (fresh ((~binder b))
                (== (#%lv-ref a) (cons (#%lv-ref b) (quote ()))))
              (== (#%lv-ref a) (cons (quote 1) (quote ())))))))


  )
