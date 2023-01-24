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

(define-struct sub-ext (binds ext))

(define (empty-subst ext-vars)
  (mark-vars-ext
   (make-immutable-free-id-table)
   (make-immutable-free-id-table)
   ext-vars))

(define (mark-vars-ext binds ext? new-ext-vars)
  (define ext?^
    (for/fold ((ext? ext?))
              ((v (in-list new-ext-vars)))
      (free-id-table-set ext? v #t)))
  (sub-ext binds ext?^))

(define (ext-subst u v s)
  (sub-ext
   (free-id-table-set (sub-ext-binds s) u v)
   (sub-ext-ext s)))

(struct level-dict (dict next-lev next-0idx))

(define (add-var-debruijn-level u v s) (free-id-table-set s u v))
(define (var-debruijn-level-get s v) (free-id-table-ref s v #f))

(define (walk t s)
  (let rec ([t t])
    (syntax-parse t
      #:literal-sets (mk-literals)
      [(#%lv-ref v)
       (let ([val (free-id-table-ref (sub-ext-binds s) #'v #f)])
         (if val (rec val) t))]
      [_ t])))

(define (walk-to-last-var t s)
  (let rec ([t t])
    (syntax-parse t
      #:literal-sets (mk-literals)
      [(#%lv-ref v)
       (let ([val (free-id-table-ref (sub-ext-binds s) #'v #f)])
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

(define (db<=/ext ext ld p1 p2)
  (define dl1 (var-debruijn-level-get (level-dict-dict ld) p1))
  (define dl2 (var-debruijn-level-get (level-dict-dict ld) p2))
  (define ext?1 (free-id-table-ref ext p1 #f))
  (define ext?2 (free-id-table-ref ext p2 #f))
  (cond
    [(and ext?1 ext?2) (db<= dl1 dl2)]
    [(not (or ext?1 ext?2)) (db<= dl1 dl2)]
    [else ext?1]))

(define (unify u v s ld)
  (let ([u^ (walk u s)]
        [v^ (walk v s)])
    (syntax-parse (list u^ v^)
      #:literal-sets (mk-literals)
      #:literals (cons quote)
      [_ #:when (equal-vals? u^ v^) (values #'(success) s ld)]
      [((#%lv-ref id1:id) (#%lv-ref id2:id))
       (if (db<=/ext (sub-ext-ext s) ld #'id1 #'id2)
           (values #`(== (#%lv-ref id2) #,(maybe-inline u u^ s)) (ext-subst #'id2 u s) ld)
           (values #`(== (#%lv-ref id1) #,(maybe-inline v v^ s)) (ext-subst #'id1 v s) ld))]
      [((#%lv-ref id1:id) _)
       (values #`(== (#%lv-ref id1) #,(maybe-inline v v^ s)) (ext-subst #'id1 v s) ld)]
      [(_ (#%lv-ref id2:id))
       (values #`(== (#%lv-ref id2) #,(maybe-inline u u^ s)) (ext-subst #'id2 u s) ld)]
      [((cons a1 d1) (cons a2 d2))
       (let*-values ([(g1^ s^ ld^) (unify #'a1 #'a2 s ld)]
                     [(g2^ s^^ ld^^) (unify #'d1 #'d2 s^ ld^)])
         (values #`(conj #,g1^ #,g2^) s^^ ld^^))]
      [((rkt-term _) _) (values #`(== #,u^ #,v^) s ld)]
      [(_ (rkt-term _)) (values #`(== #,v^ #,u^) s ld)]
      [_ (values #'(failure) s ld)])))

(define (map-maybe-inline* subst stx)
  (stx-map (curryr maybe-inline subst) stx (stx-map (curryr walk subst) stx)))

(define (compute-levels-for dict next lov)
  (for/fold ([dict dict])
            ([v (in-list lov)]
             [i (in-naturals)])
    (add-var-debruijn-level v (cons next i) dict)))

(define (add-first-level lov)
  (level-dict (compute-levels-for (make-immutable-free-id-table) 0 lov) (add1 0) (length lov)))

(define (add-level-to old-ld lov)
  (match-define (level-dict dict next-lev next-0idx) old-ld)
  (level-dict (compute-levels-for dict next-lev lov) (add1 next-lev) next-0idx))

(define (fold/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     (let-values ([(new-g _s _ld) (fold/goal #'g (empty-subst (attribute x)) (add-first-level (attribute x)))])
       #`(ir-rel (x ...) #,new-g))]))

(define (fold/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let-values ([(new-g _s _ld) (fold/goal #'g (empty-subst (attribute q)) (add-first-level (attribute q)))])
       #`(run n (q ...) #,new-g))]
    [(run* (q ...) g)
     (let-values ([(new-g _s _ld) (fold/goal #'g (empty-subst (attribute q)) (add-first-level (attribute q)))])
       #`(run* (q ...) #,new-g))]))

;; INVARIANT: goals cannot be removed, only added (by inserting conjunctions where there were previously flat goals).
;; no-ops/successes will be removed by a future pass.
(define (fold/goal g subst ld)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:unary-constraint t)
     (values #`(c . #,(map-maybe-inline* subst #'(t))) subst ld)]
    [(== t1 t2) (unify #'t1 #'t2 subst ld)]
    [(c:binary-constraint t1 t2)
     (values #`(c . #,(map-maybe-inline* subst #'(t1 t2))) subst ld)]
    [(conj g1 g2)
     (let*-values ([(g1^ s^ ld^) (fold/goal #'g1 subst ld)]
                   [(g2^ s^^ ld^^) (fold/goal #'g2 s^ ld^)])
       (values #`(conj #,g1^ #,g2^) s^^ ld^^))]
    ;; IDEA: basically assume we don't gain any subst information from disjunction because it could be self-contradictory
    [(disj g1 g2)
     (let-values ([(g1^ _s1 _ld1) (fold/goal #'g1 subst ld)]
                  [(g2^ _s2 _ld2) (fold/goal #'g2 subst ld)])
       (values #`(disj #,g1^ #,g2^) subst ld))]
    ;; Assume we don't gain any subst information within a fresh because we can't inline terms that may
    ;; refer to a variable in the fresh into contexts outside of the fresh. This is an unfortunate
    ;; limitation and should be alleviated by a better design.
    [(fresh (x ...) g)
     (let-values ([(g^ s^ ld^) (fold/goal #'g subst (add-level-to ld (attribute x)))])
       (values #`(fresh (x ...) #,g^) subst ld^))]
    [(#%rel-app n t ...)
     (with-syntax ([(u ...) (map-maybe-inline* subst #'(t ...))])
       (let ((subst^ (foldr mark-ext* subst (syntax->list #'(u ...)))))
         (values #`(#%rel-app n . (u ...)) subst^ ld)))]
    [(apply-relation e t ...)
     (with-syntax ([(u ...) (map-maybe-inline* subst #'(t ...))])
       (let ((subst^ (foldr mark-ext* subst (syntax->list #'(u ...)))))
         (values #`(apply-relation e . (u ...)) subst^ ld)))]))

(define (mark-ext* t^ subst)
  (let ((t^ (walk t^ subst)))
    (syntax-parse t^
      #:literal-sets (mk-literals)
      #:literals (cons quote)
      [(quote vl) subst]
      [(#%lv-ref v)
       (mark-vars-ext
        (sub-ext-binds subst)
        (sub-ext-ext subst)
        (list #'v))]
      [(cons t1 t2)
       (mark-ext* #'t2 (mark-ext* #'t1 subst))]
      [(rkt-term _) subst])))

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
     (ir-rel ((~binder result1))
       (fresh ((~binder x))
          (conj
           (== (#%lv-ref result1) (cons (quote (~dat-lit cat)) (quote (~dat-lit fish))))
           (== (#%lv-ref result1) (cons (quote (~dat-lit cat)) (quote (~dat-lit fish)))))))))
    (generate-prog
     (ir-rel ((~binder result1))
        (fresh ((~binder x))
          (conj
           (== (#%lv-ref result1) (cons (quote (~dat-lit cat)) (quote (~dat-lit fish))))
           (success))))))

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
          (fresh ((~binder j1))
            (fresh ((~binder j2))
              (conj
                (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j1)))
                (== (#%lv-ref j1) (#%lv-ref j2))))))))
   (generate-prog
     (ir-rel ((~binder a))
       (fresh ((~binder j1))
         (fresh ((~binder j2))
           (conj
             (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j1)))
             (== (#%lv-ref j2) (#%lv-ref j1)))))))))

(let ([foo 5])
  (progs-equal?
   (fold/rel
    (generate-prog
     (ir-rel ((~binder a))
       (fresh ((~binder j1))
         (fresh ((~binder j2))
           (conj
             (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j1)))
             (== (#%lv-ref j2) (#%lv-ref j1))))))))
(generate-prog
     (ir-rel ((~binder a))
       (fresh ((~binder j1))
         (fresh ((~binder j2))
           (conj
             (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j1)))
             (== (#%lv-ref j2) (#%lv-ref j1)))))))))

(let ([foo 5])
  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder a))
          (fresh ((~binder j1))
            (fresh ((~binder j2))
              (conj
                (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j2)))
                (== (#%lv-ref j1) (#%lv-ref j2))))))))
    (generate-prog
      (ir-rel ((~binder a))
        (fresh ((~binder j1))
          (fresh ((~binder j2))
            (conj
              (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j2)))
              (== (#%lv-ref j1) (#%lv-ref j2)))))))))

(let ([foo 5])
  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binder a))
          (fresh ((~binder j1))
            (fresh ((~binder j2))
              (conj
                (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j2)))
                (== (#%lv-ref j2) (#%lv-ref j1))))))))
    (generate-prog
      (ir-rel ((~binder a))
        (fresh ((~binder j1))
          (fresh ((~binder j2))
            (conj
              (#%rel-app foo (cons (quote (~dat-lit cat)) (#%lv-ref j2)))
              (== (#%lv-ref j1) (#%lv-ref j2)))))))))


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


;; This failing test should demonstrate a limitation in my current
;; approach.
;;
;; I had not counted on unifications underneath a rel-app, and because
;; we are not doing the reorder conj, that’s no longer a fair
;; assumption.
;;
;; Notice here, that, since we have rel-apps that change the expressed
;; behavior of a rel-app, and because we have an expressed.
(let ([foo 5])
  (progs-equal?
    (fold/rel
      (generate-prog
        (ir-rel ((~binders a b))
          (fresh ((~binder z))
            (fresh ((~binder x))
              (conj
                (fresh ((~binder w))
                  (conj
                    (#%rel-app foo (#%lv-ref w))
                    (== (#%lv-ref w) (#%lv-ref x))))
                (== (#%lv-ref x) (#%lv-ref z))))))))
    (generate-prog
      (ir-rel ((~binders a b))
        (fresh ((~binder z))
          (fresh ((~binder x))
            (conj
              (fresh ((~binder w))
                (conj
                  (#%rel-app foo (#%lv-ref w))
                  (== (#%lv-ref x) (#%lv-ref w))))
              (== (#%lv-ref x) (#%lv-ref z))))))))) ;; this is an artifact of info not propagating through fresh

;; Test shows you must still walk terms in subst to mark ext, b/c maybe-inline
(let ([foo 5])
  (progs-equal?
    (fold/rel
      (generate-prog
       (ir-rel ((~binder a))
         (fresh ((~binders z))
          (fresh ((~binders w x y))
            (conj
             (conj
              (== (#%lv-ref w) (cons (#%lv-ref x) (#%lv-ref y)))
              (#%rel-app foo (#%lv-ref w)))
             (== (#%lv-ref z) (#%lv-ref x))))))))
      (generate-prog
        (ir-rel ((~binder a))
           (fresh ((~binder z))
            (fresh ((~binders w x y))
              (conj
               (conj
                (== (#%lv-ref w) (cons (#%lv-ref x) (#%lv-ref y)))
                (#%rel-app foo (#%lv-ref w)))
               (== (#%lv-ref z) (#%lv-ref x)))))))))

(let ([foo 5])
  (progs-equal?
    (fold/rel
      (generate-prog
       (ir-rel ((~binder a))
          (fresh ((~binder z))
           (fresh ((~binders w x y))
             (conj
              (conj
               (== (#%lv-ref w) (cons (#%lv-ref x) (#%lv-ref y)))
               (#%rel-app foo (#%lv-ref w)))
              (== (#%lv-ref x) (#%lv-ref z))))))))
      (generate-prog
       (ir-rel ((~binder a))
          (fresh ((~binder z))
           (fresh ((~binders w x y))
             (conj
              (conj
               (== (#%lv-ref w) (cons (#%lv-ref x) (#%lv-ref y)))
               (#%rel-app foo (#%lv-ref w)))
              (== (#%lv-ref z) (#%lv-ref x)))))))))

)
