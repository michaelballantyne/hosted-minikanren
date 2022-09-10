#lang racket/base

(require syntax/parse
         syntax/parse/define
         syntax/id-table
         syntax/stx
         (only-in racket/match match)
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt"
         (only-in "prop-vars.rkt" SKIP-CHECK))

(provide mark-redundant-check/rel
         mark-redundant-check/run)

;; TODO docs on the conventions, names, and high-level idea, since this is non-trivial

;; fresh corresponds to a newly introduced logic variable that we are certain is not
;; assigned to any other value
(struct fresh ())

;; external corresponds to a logic variable that has come from 'the outside',
;; e.g. relation parameters, and for which we can make fewer assumptions
(struct external ())

;; top corresponds to a logic variable that could potentially be assigned to any value
(struct top ())

;; (-> any? bool?)
(define (var-val? v)
  (or (fresh? v)
      (external? v)
      (top? v)))

;; return true if the first variable is 'greater' than the other, i.e. higher in the lattice
;; (-> var-val? var-val? bool?)
(define (greater-var-val? v1 v2)
  (or (and (external? v1) (fresh? v2))
      (and (top? v1) (not (top? v2)))))

(define-syntax-parse-rule (stx-fold proc init lst ...+)
  (foldl proc init (syntax->list lst) ...))

;;;;;;;;;;;;;;;;; ABSTRACT UNIFY IMPLEMENTATION ;;;;;;;;;;;;;;;;;;;;;;
;;; logic variable helpers ;;;

(define (var? x)
  (identifier? x))

;; check if two variables are equal
;; (-> var? var? bool?)
(define (var-eq? x y)
  (free-identifier=? x y))

;; get associated value to variable
;; ASSUMPTION: this is called on a variable that is leaf-var?
;; (-> var? subst? var-val?)
(define (get-val x s)
  (subst-lookup s x))

;; return true if variable is a fresh leaf variable
;; (-> var? subst? bool?)
(define (fresh-var? x s)
  (fresh? (subst-lookup s x)))

;; return true if variable is an external leaf variable
;; (-> var? subst? bool?)
(define (external-var? x s)
  (external? (subst-lookup s x)))

;; return true if variable is a top leaf variable
;; (-> var? subst? bool?)
(define (top-var? x s)
  (top? (subst-lookup s x)))

;; return true if the variable isn't assigned to a concrete term in the substitution
;; (-> var? subst? bool?)
(define (leaf-var? x s)
  (var-val? (subst-lookup s x)))

;; add a logic variable to the substitution assigned to a term
;; (-> subst? var? term? subst?)
(define (add-var s lv v)
  (free-id-table-set s lv v))

;;; term/substitution helpers ;;;

;; return the value associated with a given variable in a substitution
;; ASSUMES that the key (variable) exists
;; (-> subst? var? term?)
(define (subst-lookup s x)
  (free-id-table-ref s x))

;; (-> (or term? var-val?) boolean?)
(define (ground? t s)
  (syntax-parse t
    #:literal-sets (mk-literals)
    #:literals (cons quote)
    [(#%lv-ref _) #f]
    [(cons a d) (and (ground? #'a s) (ground? #'d s))]
    [_ #t]))

;; determine if a logic variable appears in a term w.r.t. a given substitution
;; (-> term? var? subst? boolean?)
(define (contains? t x s)
  (syntax-parse t
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%lv-ref v)
     (cond
       [(var-eq? x #'v) #t]
       [(not (leaf-var? #'v s))
        (contains? (walk t s) x s)]
       [else #f])]
    [(cons a d)
     (or (contains? #'a x s)
         (contains? #'d x s))]
    [_ #f]))

;; TODO think more carefully about how different functions should be categorized
;;; unify helpers ;;;

;; update or add new binding for logic variable to term in substitution
;; (-> subst? var? term? subst?) 
(define (modify-subst s x t)
  (free-id-table-set s x t))

;; resolve all non-leaf variables in a term to their values, recursively
;; (-> term? subst? term?)

(define (walk t s)
  (define (walk-term t)
    (syntax-parse t
      #:literal-sets (mk-literals)
      [(#%lv-ref v)
       (let ([res (walk-id #'v)])
         (if (var? res)
           #`(#%lv-ref #,res)
           res))]
      [_ t]))

  (define (walk-id id)
    (let ([val (subst-lookup s id)])
      (if (var-val? val)
        id
        (walk-term val))))

  (syntax-parse t
    #:literal-sets (mk-literals)
    [v:id (walk-term #'(#%lv-ref v))]
    [_ (walk-term t)]))

;; promote all leaf variables in a term to a var-val
;; (-> term? var-val? subst? subst?)
(define (color t v s)
  (syntax-parse t
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%lv-ref lv)
     (cond
       [(and (leaf-var? #'lv s) (greater-var-val? v (get-val #'lv s)))
        (modify-subst s #'lv v)]
       [(not (leaf-var? #'lv s)) (color (walk t s) v s)]
       [else s])]
    [(cons a d)
     (color #'d v (color #'a v s))]
    [_ s]))

;; return a table mapping variables that appear in a term to the number of times they appear
;; (-> term? subst? (hash-of var? number?))
(define (get-var-counts t s)
  (define (inc counts v)
    (free-id-table-update counts v add1 0))
  
  ;; (-> term? subst? (hash-of var? number?) (hash? var? number?))
  (define (counts/acc t s counts)
    (syntax-parse t
      #:literal-sets (mk-literals)
      #:literals (quote cons)
      [(#%lv-ref v)
       (if (leaf-var? #'v s)
         (inc counts #'v)
         (counts/acc (walk t s) s counts))]
      [(cons a d)
       (counts/acc #'d
                   s
                   (counts/acc #'a s counts))]
      [_ counts]))

  (counts/acc t s (make-immutable-free-id-table)))

(define (free-id-table->list table)
  (for/list ([(k v) (in-free-id-table table)])
    (cons k v)))

;; return if any of the following conditions are true:
;; - any variable appears more than once
;; - a variable is classified as external
;; - a variable is classified as top
;; any of which could contribute to the occurs check failing
;; (-> term? subst? boolean?)
(define (can-cause-failure? t s)
  (define vcs (get-var-counts t s))
  (ormap (λ (pair) (or (external-var? (car pair) s)
                       (top-var? (car pair) s)
                       (> (cdr pair) 1)))
         (free-id-table->list vcs)))

;;; Disjunction Helpers ;;;

(define (join t1 t2 s1 s2)
  (syntax-parse (list t1 t2)
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [_ #:when (equal? t1 t2) t1]
    [((#%lv-ref lv1) (#%lv-ref lv2))
     (if (and (leaf-var? #'lv1 s1) (leaf-var? #'lv2 s2))
       (if (greater-var-val? #'lv1 #'lv2)
         (get-val #'lv1 s1)
         (get-val #'lv2 s2))
       (join (walk t1 s1) (walk t2 s2) s1 s2))]
    [_ (top)]))

;;; API ;;;
(define (empty-subst) (make-immutable-free-id-table))

(define (add-fresh-vars s vars)
  (stx-fold (λ (lv s) (add-var s lv (fresh))) s vars))

(define (mark-terms-external s terms)
  (stx-fold (λ (t s) (color t (external) s)) s terms))

(define (mark-vars-external s vars)
  (mark-terms-external s #`#,(stx-map (λ (v) #`(#%lv-ref #,v)) vars)))

(define (mark-subst-top s)
  (for/fold ([s^ (make-immutable-free-id-table)])
            ([(k v) (in-free-id-table s)])
    (free-id-table-set s^ k (if (var-val? v) (top) v))))

(define-syntax-class atomic
  #:literal-sets (mk-literals)
  #:literals (quote)
  (pattern (~or (#%term-datum v) (quote v))))

(define (unify u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (syntax-parse (list u v)
      #:literal-sets (mk-literals)
      #:datum-literals (quote cons)
      [(_:atomic _:atomic) (values s #t)]
      [(~or (a:atomic (#%lv-ref lv))
            ((#%lv-ref lv) a:atomic))
       (values (modify-subst s #'lv #'a.v) #t)]
      [(~or (_:atomic (cons _ _))
            ((cons _ _) _:atomic))
       (values s #t)]
      [((#%lv-ref lv1) (#%lv-ref lv2))
       (cond
         [(var-eq? #'lv1 #'lv2) (values s #t)]
         [(and (fresh-var? #'lv1 s) (fresh-var? #'lv2 s))
          (values (modify-subst s #'lv1 v) #t)]
         [(and (fresh-var? #'lv1 s) (external-var? #'lv2 s))
          (values (modify-subst s #'lv1 v) #t)]
         [(and (fresh-var? #'lv1 s) (top-var? #'lv2 s))
          (values (modify-subst s #'lv2 u) #f)]
         [(and (external-var? #'lv1 s) (fresh-var? #'lv2 s))
          (values (modify-subst s #'lv2 u) #t)]
         [(and (external-var? #'lv1 s) (external-var? #'lv2 s))
          (values (modify-subst s #'lv1 v) #f)]
         [(and (external-var? #'lv1 s) (top-var? #'lv2 s))
          (values (modify-subst s #'lv1 v) #f)]
         [(and (top-var? #'lv1 s) (fresh-var? #'lv2 s))
          (values (modify-subst s #'lv2 u) #f)]
         [(and (top-var? #'lv1 s) (external-var? #'lv2 s))
          (values (modify-subst s #'lv2 u) #f)]
         [(and (top-var? #'lv1 s) (top-var? #'lv2 s))
          (values (modify-subst s #'lv1 v) #f)]
         [else (raise "NOT POSSIBLE, NOT A VARIABLE VALUE")])]
      [(~or ((#%lv-ref lv) (cons a d))
            ((cons a d) (#%lv-ref lv)))
       (define t #'(cons a d))
       (cond
         [(contains? t #'lv s) (values s #f)]
         [(ground? t s) (values (modify-subst s #'lv t) #t)]
         [(and (or (external-var? #'lv s) (top-var? #'lv s))
               (can-cause-failure? t s))
          (values (modify-subst (color t (get-val #'lv s) s) #'lv t) #f)]
         [else
          (values (modify-subst (color t (get-val #'lv s) s) #'lv t) #t)])]
      [((cons a1 d1) (cons a2 d2))
       (let*-values ([(s^ left-can) (unify #'a1 #'a2 s)]
                     [(s^^ right-can) (unify #'d1 #'d2 s^)])
         (values s^^ (and left-can right-can)))]
      [(~or ((rkt-term _) _)
            (_ (rkt-term _)))
       (values (mark-subst-top s) #f)]
      ;; TODO what do in presence of `rkt-term`?
      [_ (raise "ALL TERM COMBINATIONS ALREADY HANDLED, NOT REACHABLE")])))

(define (union s1 s2)
  (for/fold ([s s1])
            ([(k v) (in-free-id-table s2)])
    (let ([curr-v (free-id-table-ref s k #f)])
      (if curr-v
        (free-id-table-set s k (join curr-v v s1 s2)) ;; FIXME should this be s or s1 as the first subst?
        (free-id-table-set s k v)))))

;;;;;;;;;;;;;;;;;;;;;; EXTERNAL API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mark-redundant-check/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     (let* ([vars #'(x ...)]
            [s (empty-subst)]
            [s^ (add-fresh-vars s vars)]
            [s^^ (mark-vars-external s^ vars)])
       (define-values (new-g _) (mark-redundant-check/goal #'g s^^))
       #`(ir-rel (x ...) #,new-g))]))

(define (mark-redundant-check/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let* ([vars #'(q ...)]
            [s (empty-subst)]
            [s^ (add-fresh-vars s vars)])
       (define-values (new-g _) (mark-redundant-check/goal #'g s^))
       #`(run n (q ...) #,new-g))]
    [(run* (q ...) g)
     (let* ([vars #'(q ...)]
            [s (empty-subst)]
            [s^ (add-fresh-vars s vars)])
       (define-values (new-g _) (mark-redundant-check/goal #'g s^))
       #`(run* (q ...) #,new-g))]))

;;;;;;;;;;;;;;;;;;;;;; PASS IMPLEMENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mark-redundant-check/goal g s)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:nullary-constraint) (values this-syntax s)]
    [(c:unary-constraint t) (values this-syntax s)]
    [(== t1 t2)
     (let-values ([(s^ can-skip?) (unify #'t1 #'t2 s)])
       (values
         (if can-skip?
           (syntax-property this-syntax SKIP-CHECK #t)
           this-syntax)
         s^))]
    [(c:binary-constraint t1 t2) (values this-syntax s)] ;; TODO do any other binary constraints make the pass unsound?
    [(conj g1 g2)
     (let*-values ([(g1^ s^) (mark-redundant-check/goal #'g1 s)]
                   [(g2^ s^^) (mark-redundant-check/goal #'g2 s^)])
       (values #`(conj #,g1^ #,g2^) s^^))] 
    [(disj g1 g2)
     (let-values ([(g1^ s1) (mark-redundant-check/goal #'g1 s)]
                  [(g2^ s2) (mark-redundant-check/goal #'g2 s)])
       (values #`(disj #,g1^ #,g2^) (union s1 s2)))]
    [(fresh (x ...) g)
     (let-values ([(new-g s^) (mark-redundant-check/goal #'g (add-fresh-vars s #'(x ...)))])
       (values #`(fresh (x ...) #,new-g) s^))]
    [(#%rel-app n t ...) (values this-syntax (mark-terms-external s #'(t ...)))]
    [(apply-relation e t ...) (values this-syntax (mark-terms-external s #'(t ...)))]))

;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* test racket/base
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (only-in "prop-vars.rkt" SKIP-CHECK)
                       (submod "..")))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (#%lv-ref q) (cons (#%term-datum 1) (#%term-datum 2))))))
    (generate-prog
      (ir-rel ((~binder p))
        (~check (== (#%lv-ref p) (cons (#%term-datum 1) (#%term-datum 2))) SKIP-CHECK))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (== (#%lv-ref q) (#%lv-ref p)))))
    (generate-prog
      (ir-rel ((~binders q p))
        (~missing (== (#%lv-ref q) (#%lv-ref p)) SKIP-CHECK))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (== (#%lv-ref x) (#%lv-ref q))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (~check (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK)))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (== (#%lv-ref x) (#%lv-ref y))
              (== (#%lv-ref y) (#%lv-ref q)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (~check (== (#%lv-ref x) (#%lv-ref y)) SKIP-CHECK)
            (~check (== (#%lv-ref y) (#%lv-ref q)) SKIP-CHECK))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (== (#%lv-ref q) (cons (#%term-datum 5) (#%term-datum 5))))))
    (generate-prog
      (ir-rel ((~binder q))
        (~check (== (#%lv-ref q) (cons (#%term-datum 5) (#%term-datum 5))) SKIP-CHECK))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (== (#%lv-ref y) (#%term-datum 5))
              (conj
                (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
                (== (#%lv-ref x) (#%lv-ref q))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (~check (== (#%lv-ref y) (#%term-datum 5)) SKIP-CHECK)
            (conj
              (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
              (~check (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (== (#%lv-ref x) (#%lv-ref q)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (~check (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (== (#%lv-ref y) (#%lv-ref q)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (~check (== (#%lv-ref y) (#%lv-ref q)) SKIP-CHECK))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (== (#%lv-ref q) (cons (#%lv-ref p) '())))))
    (generate-prog
      (ir-rel ((~binders q p))
        (~missing (== (#%lv-ref q) (cons (#%lv-ref p) '())) SKIP-CHECK))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%lv-ref y)))
              (== (#%lv-ref x) (#%lv-ref q)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%lv-ref y))) SKIP-CHECK)
            (~missing (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (fresh ((~binders x y z))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (conj
                (== (#%lv-ref x) (cons (#%lv-ref p) (#%lv-ref z)))
                (== (#%lv-ref q) (#%lv-ref x))))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (fresh ((~binders x y z))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (conj
              (~check (== (#%lv-ref x) (cons (#%lv-ref p) (#%lv-ref z))) SKIP-CHECK)
              (~missing (== (#%lv-ref q) (#%lv-ref x)) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y z))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (conj
                (== (#%lv-ref x) (cons (cons (#%lv-ref z) (#%lv-ref z)) (#%term-datum 5)))
                (== (#%lv-ref y) (#%lv-ref q))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y z))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (conj
              (~check (== (#%lv-ref x) (cons (cons (#%lv-ref z) (#%lv-ref z)) (#%term-datum 5))) SKIP-CHECK)
              (~missing (== (#%lv-ref y) (#%lv-ref q)) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binders q p))
          (conj
            (== (#%lv-ref q) (cons (#%term-datum 5) (#%term-datum 5)))
            (== (#%lv-ref q) (#%lv-ref p))))))
    (generate-prog
      (ir-rel ((~binders q p))
        (conj
          (~check (== (#%lv-ref q) (cons (#%term-datum 5) (#%term-datum 5))) SKIP-CHECK)
          (~check (== (#%lv-ref q) (#%lv-ref p)) SKIP-CHECK)))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y z))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (conj
                (== (#%lv-ref x) (cons (#%lv-ref z) (#%term-datum 5)))
                (== (#%lv-ref x) (cons (#%lv-ref q) (#%term-datum 5)))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y z))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (conj
              (~check (== (#%lv-ref x) (cons (#%lv-ref z) (#%term-datum 5))) SKIP-CHECK)
              (~check (== (#%lv-ref x) (cons (#%lv-ref q) (#%term-datum 5))) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders w x y z))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (conj
                (== (#%lv-ref x) (cons (cons (#%lv-ref z) (#%lv-ref z)) (#%term-datum 5)))
                (conj
                  (== (#%lv-ref z) (cons (#%lv-ref w) (#%lv-ref w)))
                  (== (#%lv-ref x) (cons (cons (#%lv-ref q) (#%lv-ref q)) (#%term-datum 5))))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders w x y z))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (conj
              (~check (== (#%lv-ref x) (cons (cons (#%lv-ref z) (#%lv-ref z)) (#%term-datum 5))) SKIP-CHECK)
              (conj
                (~check (== (#%lv-ref z) (cons (#%lv-ref w) (#%lv-ref w))) SKIP-CHECK)
                (~missing (== (#%lv-ref x) (cons (cons (#%lv-ref q) (#%lv-ref q)) (#%term-datum 5))) SKIP-CHECK))))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y z))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (conj
                (== (#%lv-ref x) (cons (#%lv-ref z) (#%term-datum 5)))
                (== (#%lv-ref q) (cons (#%lv-ref z) (#%term-datum 6)))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y z))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (conj
              (~check (== (#%lv-ref x) (cons (#%lv-ref z) (#%term-datum 5))) SKIP-CHECK)
              (~check (== (#%lv-ref q) (cons (#%lv-ref z) (#%term-datum 6))) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y z))
            (conj
              (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
              (conj
                (== (#%lv-ref x) (cons (#%lv-ref z) (#%term-datum 5)))
                (== (#%lv-ref q) (cons (#%lv-ref z) (#%lv-ref x)))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y z))
          (conj
            (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
            (conj
              (~check (== (#%lv-ref x) (cons (#%lv-ref z) (#%term-datum 5))) SKIP-CHECK)
              (~missing (== (#%lv-ref q) (cons (#%lv-ref z) (#%lv-ref x))) SKIP-CHECK)))))))

  (let ([foo 5])
    (progs-equal?
      (mark-redundant-check/rel
        (generate-prog
          (ir-rel ((~binder q))
            (fresh ((~binders x y))
              (conj
                (== (#%lv-ref x) (#%lv-ref y))
                (conj
                  (#%rel-app foo (#%lv-ref x))
                  (== (#%lv-ref y) (#%lv-ref q))))))))
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (~check (== (#%lv-ref x) (#%lv-ref y)) SKIP-CHECK)
              (conj
                (#%rel-app foo (#%lv-ref x))
                (~missing (== (#%lv-ref y) (#%lv-ref q)) SKIP-CHECK))))))))

  (let ([foo 5])
    (progs-equal?
      (mark-redundant-check/rel
        (generate-prog
          (ir-rel ((~binder q))
            (fresh ((~binders x y))
              (conj
                (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
                (conj
                  (#%rel-app foo (#%lv-ref y))
                  (== (#%lv-ref x) (#%lv-ref q))))))))
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
              (conj
                (#%rel-app foo (#%lv-ref y))
                (~missing (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK))))))))

  (let ([foo 5])
    (progs-equal?
      (mark-redundant-check/rel
        (generate-prog
          (ir-rel ((~binder q))
            (fresh ((~binders x y))
              (conj
                (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5)))
                (conj
                  (== (#%lv-ref y) (#%term-datum 5))
                  (conj
                    (#%rel-app foo (#%lv-ref y))
                    (== (#%lv-ref x) (#%lv-ref q)))))))))
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (~check (== (#%lv-ref x) (cons (#%lv-ref y) (#%term-datum 5))) SKIP-CHECK)
              (conj
                (~check (== (#%lv-ref y) (#%term-datum 5)) SKIP-CHECK)
                (conj
                  (#%rel-app foo (#%lv-ref y))
                  (~check (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK)))))))))

  (let ([foo 5])
    (progs-equal?
      (mark-redundant-check/rel
        (generate-prog
          (ir-rel ((~binder q))
            (fresh ((~binders w x y z))
              (conj
                (== (#%lv-ref w) (cons (#%lv-ref x) (#%lv-ref y)))
                (conj
                  (#%rel-app foo (cons (#%term-datum 5) (#%lv-ref w)))
                  (== (#%lv-ref y) (#%lv-ref q))))))))
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders w x y z))
            (conj
              (~check (== (#%lv-ref w) (cons (#%lv-ref x) (#%lv-ref y))) SKIP-CHECK)
              (conj
                (#%rel-app foo (cons (#%term-datum 5) (#%lv-ref w)))
                (~missing (== (#%lv-ref y) (#%lv-ref q)) SKIP-CHECK))))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders w x y z))
            (== (#%lv-ref w) (cons (#%lv-ref w) (#%lv-ref w)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders w x y z))
          (~missing (== (#%lv-ref w) (cons (#%lv-ref w) (#%lv-ref w))) SKIP-CHECK)))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binder x))
            (conj
              (== (#%lv-ref q) (cons (#%lv-ref x) '()))
              (disj
                (== (#%lv-ref x) (#%term-datum 5))
                (== (#%lv-ref x) (#%term-datum 6))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binder x))
          (conj
            (~check (== (#%lv-ref q) (cons (#%lv-ref x) '())) SKIP-CHECK)
            (disj
              (~check (== (#%lv-ref x) (#%term-datum 5)) SKIP-CHECK)
              (~check (== (#%lv-ref x) (#%term-datum 6)) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (disj
                (== (#%lv-ref x) (#%lv-ref q))
                (== (#%lv-ref y) (#%lv-ref q)))
              (conj
                (== (#%lv-ref x) (#%term-datum 5))
                (== (#%lv-ref q) (cons (#%lv-ref y) '()))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (disj
              (~check (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK)
              (~check (== (#%lv-ref y) (#%lv-ref q)) SKIP-CHECK))
            (conj
              (~check (== (#%lv-ref x) (#%term-datum 5)) SKIP-CHECK)
              (~missing (== (#%lv-ref q) (cons (#%lv-ref y) '())) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y))
            (conj
              (disj
                (== (#%lv-ref x) (#%lv-ref q))
                (== (#%lv-ref y) (#%term-datum 5)))
              (conj
                (== (#%lv-ref y) (#%lv-ref q))
                (== (#%lv-ref q) (cons (#%lv-ref y) '()))))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y))
          (conj
            (disj
              (~check (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK)
              (~check (== (#%lv-ref y) (#%term-datum 5)) SKIP-CHECK))
            (conj
              (~missing (== (#%lv-ref y) (#%lv-ref q)) SKIP-CHECK)
              (~missing (== (#%lv-ref q) (cons (#%lv-ref y) '())) SKIP-CHECK)))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y z))
            (conj
              (disj
                (== (#%lv-ref x) (#%lv-ref q))
                (== (#%lv-ref x) (#%lv-ref y)))
              (== (#%lv-ref x) (#%lv-ref z)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y z))
          (conj
            (disj
              (~check (== (#%lv-ref x) (#%lv-ref q)) SKIP-CHECK)
              (~check (== (#%lv-ref x) (#%lv-ref y)) SKIP-CHECK))
            (~check (== (#%lv-ref x) (#%lv-ref z)) SKIP-CHECK))))))

  (progs-equal?
    (mark-redundant-check/rel
      (generate-prog
        (ir-rel ((~binder q))
          (fresh ((~binders x y z a))
            (conj
              (conj
                (== (#%lv-ref x) (#%term-datum 5))
                (== (#%lv-ref y) (rkt-term 6)))
              (== (#%lv-ref z) (#%lv-ref a)))))))
    (generate-prog
      (ir-rel ((~binder q))
        (fresh ((~binders x y z a))
          (conj
            (conj
              (~check (== (#%lv-ref x) (#%term-datum 5)) SKIP-CHECK)
              (~missing (== (#%lv-ref y) (rkt-term 6)) SKIP-CHECK))
            (~missing (== (#%lv-ref z) (#%lv-ref a)) SKIP-CHECK))))))

  )