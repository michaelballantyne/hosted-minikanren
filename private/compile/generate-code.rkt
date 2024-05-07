#lang racket/base

(require (for-template (prefix-in mk: "../../mk/main.rkt")
                       racket/base
                       racket/stxparam
                       (prefix-in mku: "../../mk/private-unstable.rkt")
                       "../forms.rkt"
                       "../runtime.rkt")
         "prop-vars.rkt"
         "utils.rkt"
         ee-lib
         "../forms.rkt"
         "test/unit-test-progs.rkt"
         syntax/id-table
         "./macro-scopes-bound-id-set.rkt"
         syntax/parse
         (only-in syntax/parse
                  (define/syntax-parse def/stx))
         syntax/stx
         "../syntax-classes.rkt")

(provide generate-goal/entry generate-relation generate-term)


(define specialize-unify? (make-parameter #t))

(define (generate-goal/entry g specialize?)
  (parameterize ([specialize-unify? specialize?])
    (generate-goal g)))

(define/hygienic (generate-relation stx) #:expression
  (syntax-parse stx
    [(_ (x^ ...) g^)
     #`(lambda (x^ ...) #,(generate-goal #'g^))]))

(define constraint-impls
  (make-free-id-table
   (hash #'symbolo #'mk:symbolo
         #'stringo #'mk:stringo
         #'numbero #'mk:numbero
         #'== #'mk:==
         #'=/= #'mk:=/=
         #'absento #'mk:absento)))

(define (collect-disjs stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(disj g1 g2) (cons #'g1 (collect-disjs #'g2))]
    [_ (list this-syntax)]))

;; IRGoal RTGoal -> RTGoal
;; If any term in the goal contains a term-from-expression, then
;; we need to eta-expand the goal to access the current state
;; and set the surrounding state var syntax parameter
(define (maybe-bind-surrounding-current-state-var g generated-goal)
  (if (contains-term-from-expression? g)
      #`(λ (st)
          ((syntax-parameterize ([surrounding-current-state-var #'st])
             #,generated-goal)
           st))
      generated-goal))

(define/hygienic (generate-goal stx) #:expression
  (syntax-parse stx
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [succeed #'mku:succeed]
    [fail #'mku:fail]
    [(c:unary-constraint t)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     (maybe-bind-surrounding-current-state-var stx #`(c^ #,(generate-term #'t)))]
    [(== t1 t2)
     (maybe-bind-surrounding-current-state-var stx (generate-== stx))]
    [(c:binary-constraint t1 t2)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     (maybe-bind-surrounding-current-state-var stx #`(c^ #,(generate-term #'t1) #,(generate-term #'t2)))]
    [(#%rel-app n:id t ...)
     (maybe-bind-surrounding-current-state-var stx #`(n #,@ (stx-map generate-term #'(t ...))))]
    [(disj g1 g2)
     #`(mk:conde
         #,@(stx-map (compose list generate-goal) (collect-disjs this-syntax)))]
    [(conj g1 g2)
     #`(mku:conj #,(generate-goal #'g1)
                 #,(generate-goal #'g2))]
    [(fresh (x:id ...) g)
     (if (specialize-unify?)
       (compile-block #'(x ...) #'g)
       #`(mk:fresh (x ...) #,(generate-goal #'g)))]
    [(goal-from-expression e)
     #:with (var-in-scope ...) (map flip-intro-scope (syntax-property this-syntax TERM-VARS-IN-SCOPE))
     #'(λ (st)
         (unseal-and-apply-goal (syntax-parameterize ([surrounding-current-state-var #'st]
                                                      [surrounding-term-vars-in-scope (list #'var-in-scope ...)])
                                  e)
                                #'e
                                st))]
    [(apply-relation e t ...)
     (maybe-bind-surrounding-current-state-var stx
                                               #`((relation-value-proc (check-relation e #'e))
                                                  #,@(stx-map generate-term #'(t ...))))]))

(define (take-first-== g)
  (syntax-parse g
    #:context 'take-first-==
    #:literal-sets (mk-literals)
    [(== . _) #:when (not (contains-term-from-expression? g))
     (values this-syntax #f)]
    [(conj g1 g2)
     (define-values (the-== g1-rest) (take-first-== #'g1))
     (if the-==
         (if g1-rest
             (values the-== #`(conj #,g1-rest g2))
             (values the-== #'g2))
         (values #f this-syntax))]
    [_ (values #f this-syntax)]))

;; IRGoal -> (values (listof IRGoal) (listof IRGoal))
;;
;; splits the block into the prefix that we can ==-optimize an the
;; remainer that we cannot
;;
;; Note that when we have an == w/a term-from-expression in it, that
;; == too terminates the optimization, so we split the block there
(define (split-block g)
  (let loop ([g g] [==s '()])
    (define-values (the-== g-rest) (take-first-== g))
    (if the-==
        (loop g-rest (cons the-== ==s))
        (values (reverse ==s) g))))

(define (split-vars vars unify-gs)
  (define refed-vars
    (for/fold ([ids (immutable-bound-id-set)]) ([unify-g unify-gs])
      (bound-id-set-union ids (==-vars unify-g))))

  (values (bound-id-set-intersect (immutable-bound-id-set vars) refed-vars)
          (bound-id-set-subtract (immutable-bound-id-set vars) refed-vars)))

(define (term-vars t)
  (syntax-parse t
    #:context 'term-vars
    #:literal-sets (mk-literals)
    #:literals (cons)
    [(#%lv-ref v)
     (immutable-bound-id-set (list #'v))]
    [(cons t1 t2)
     (bound-id-set-union
      (term-vars #'t1)
      (term-vars #'t2))]
    [_ (immutable-bound-id-set '())]))

(define (==-vars g)
  (syntax-parse g
    #:context '==-vars
    #:literal-sets (mk-literals)
    [(== t1 t2)
     (bound-id-set-union (term-vars #'t1) (term-vars #'t2))]))

(require racket/pretty)

(define/hygienic (compile-block fresh-vars g) #:expression
  (define-values (unify-gs rest-g) (split-block g))
  (define-values (unify-vars other-vars) (split-vars (syntax->list fresh-vars) unify-gs))
  (define/syntax-parse (other-var ...) (bound-id-set->list other-vars))

  #`(mku:fresh (other-var ...)
               (lambda (st)
                 #,(compile-block-conjunction
                    #`(block (#,@unify-gs) #,rest-g) unify-vars #'st))))

(define/hygienic (compile-block-conjunction block remaining-unify-vars st) #:expression
  (syntax-parse block
    [(block () rest-g)
     (unless (bound-id-set-empty? remaining-unify-vars)
       (error 'compile-block "should be no remaining-unify-vars here"))
     (if (syntax-e #'rest-g)
         #`(#,(generate-goal #'rest-g)
            #,st)
         st)]
    [(block (unify-g1 unify-gs ...) rest-g)
     (generate-optimized-unify
       #'unify-g1
       remaining-unify-vars
       st
       #'(block (unify-gs ...) rest-g))]))


(define (continue-block-conjunction block remaining-unify-vars st)
  #`(and #,st
         #,(compile-block-conjunction block remaining-unify-vars st)))

(define (fresh-term-vars t all-fresh-vars)
  ;; we want the hygiene context of the vars in `t`.
  (immutable-bound-id-set
   (for/list ([v (in-bound-id-set (term-vars t))]
              #:when (bound-id-set-member? all-fresh-vars v))
     v)))

#|

This is used in a block of the the unifies where the LHS is a variable that we
have just introduced. So these can be "let bound", b/c we do not need
them in the subst yet; they can just be local racket vars.

t2 could be an aribtrary term, and so could have mk variables in it,
and some that we may need to freshen.

remaining unify vars: the variables introduced by the fresh in the IR
syntax for which we have not yet introduced any bindings

|#
(define/hygienic (generate-let-unify v t2 remaining-unify-vars st rest-block) #:expression
  (define t2-fresh-vars (fresh-term-vars t2 remaining-unify-vars))
  (define remaining-unify-vars^ (bound-id-set-remove
                                 (bound-id-set-subtract remaining-unify-vars t2-fresh-vars)
                                 v))
  (define/syntax-parse (t2-fresh-var ...) (bound-id-set->list t2-fresh-vars))
  #`(let ([sc (get-state-from-scope #,st)])
      (let ([t2-fresh-var (mku:var sc)] ...)
        (let ([#,v #,(generate-term t2)])
          #,(continue-block-conjunction rest-block remaining-unify-vars^ st)))))

(define/hygienic (generate-optimized-unify stx remaining-unify-vars st rest-block) #:expression
  (define no-occur? (syntax-property stx SKIP-CHECK))

  (syntax-parse stx
    #:context 'generate-optimized-unify
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(== (#%lv-ref v:id) t2)
     (cond
       [(bound-id-set-member? (term-vars #'t2) #'v)
        #'#f
        #;(error 'generate-optimized-unify "invariant violation---occurs check violation should be caught in constant folding")]
       [(bound-id-set-member? remaining-unify-vars #'v)
        (generate-let-unify #'v #'t2 remaining-unify-vars st rest-block)]
       [else
        (generate-matching-unify #'v #'t2 remaining-unify-vars st no-occur? rest-block)])]
    [(== t1 t2)
     (error 'generate-optimized-unify "invariant violation")]))

(define/hygienic (generate-matching-unify v t2 remaining-unify-vars st no-occur? rest-block) #:expression
  (define t2-fresh-vars (fresh-term-vars t2 remaining-unify-vars))
  (define remaining-unify-vars^ (bound-id-set-subtract remaining-unify-vars t2-fresh-vars))
  (define/syntax-parse (body-var ...) (bound-id-set->list t2-fresh-vars))
  #`(let ([body (lambda (body-var ... st)
                  #,(continue-block-conjunction rest-block remaining-unify-vars^ #'st))])
      #,(generate-matching-unify-body v t2 st remaining-unify-vars no-occur? #'body #'(body-var ...))))

(define/hygienic (generate-matching-unify-body v t2 st fresh-unify-vars no-occur? join-point-name join-point-vars) #:expression
  (syntax-parse t2
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%lv-ref w:id)
     #:when (bound-id-set-member? fresh-unify-vars #'w)
     #`(let ([w #,v]) (#,join-point-name #,@join-point-vars #,st))]
    [(#%lv-ref w:id)
     #:when (syntax-property this-syntax FIRST-REF)
     ;; If/when we can analyse term from expression to see its free vars, then we could use first refs prop on the term from expression,
     ;; and then we really would be able to allocate them at that point and we wouldn't need the fresh-unify-vars part here in code gen
     (error 'generate-matching-unify-body "FIRST-REF should also be a fresh-unify-var and be handled by that case.")]
    [(#%lv-ref w:id)
     #`(#,join-point-name #,@join-point-vars #,(generate-runtime-unify v #'w st no-occur?))]
    [(term-from-expression e)
     (error 'generate-matching-unify-body "invariant violation")]
    [(quote l)
     #`(let ([v^ (walk-in-state #,v #,st)])
         (cond
           [(mku:var? v^) (#,join-point-name #,@join-point-vars (mku:ext-st-check-c v^ 'l #,st))]
           ;; NB. We could specialize on the type of l here; if it's a symbol you could use eq, numbers eqv?, etc.
           [(equal? v^ 'l) (#,join-point-name #,@join-point-vars #,st)]
           [else #f]))]
    [(cons t2-a:term/c t2-b:term/c)

     (define t2-vars (fresh-term-vars t2 fresh-unify-vars))
     (define t2-a-vars (fresh-term-vars #'t2-a fresh-unify-vars))
     (define/syntax-parse (t2-var ...) (bound-id-set->list t2-vars))
     (define/syntax-parse (t2-a-var ...) (bound-id-set->list t2-a-vars))
     #`(let ([v^ (walk-in-state #,v #,st)])
         (cond
           [(mku:var? v^) (let ([t2-var (fresh-var-w-state-scope #,st)] ...)
                            (#,join-point-name #,@join-point-vars
                                               #,(generate-ext #'v^ (generate-term t2) st no-occur?)))]
           [(pair? v^)
            (let ([do-cdr (lambda (t2-a-var ... st)
                            (and st
                                 #,(generate-matching-unify-body #'(cdr v^) #'t2-b #'st fresh-unify-vars no-occur? join-point-name join-point-vars)))])
              #,(generate-matching-unify-body #'(car v^) #'t2-a st fresh-unify-vars no-occur? #'do-cdr #'(t2-a-var ...)))]
           [else #f]))]))

(define (generate-runtime-unify u v st no-occur?)
  (if no-occur?
      #`(mku:unify2-no-occur-check #,u #,v #,st)
      #`(mku:unify2 #,u #,v #,st)))

(define (generate-ext v t st no-occur?)
  (if no-occur?
      #`(mku:ext-st-check-c #,v #,t #,st)
      #`(mku:ext-st-check-occurs-check-c #,v #,t #,st)))

;; stx stx stx -> stx
(define/hygienic (generate-specialized-unify-body v t2 st no-occur?) #:expression
  (syntax-parse t2
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%lv-ref w:id)
     (let ([first-ref? (syntax-property t2 FIRST-REF)])
       (cond
         [first-ref? #`(let ([v^ (walk-in-state #,v #,st)])
                         (mku:ext-st-no-check w v^ #,st))]
         [no-occur?
          #`(mku:unify2-no-occur-check #,v w #,st)]
         [else
          #`(mku:unify2 #,v w #,st)]))]
    [(term-from-expression e)
     (if no-occur?
         #`(unify2/no-occur-check/check-unseal-vars #,v e #'e #,st)
         #`(unify2/check-unseal-vars #,v e #'e #,st))]
    [(quote l)
     #`(let ([v^ (walk-in-state #,v #,st)])
         (let ([t (quote l)])
         (cond
             [(equal? v^ t) #,st]
             [(mku:var? v^) (mku:ext-st-check-c v^ t #,st)]
             [else #f])))]
    [(cons t2-a:term/c t2-b:term/c)
     #`(let ([v^ (walk-in-state #,v #,st)])
         (cond
           [(mku:var? v^)
            (let ([t #,(generate-term t2)])
              #,(if no-occur?
                    #`(mku:ext-st-check-c v^ t #,st)
                    #`(mku:ext-st-check-occurs-check-c v^ t #,st)))]
           [(pair? v^)
            (let ([st^ #,(generate-specialized-unify-body #'(car v^) #'t2-a st no-occur?)])
              (and st^
                   #,(generate-specialized-unify-body #'(cdr v^) #'t2-b #'st^ no-occur?)))]
           [else #f]))]))

;; stx stx -> stx
(define/hygienic (generate-specialized-unify v t2 no-occur?) #:expression
  #`(λ (st)
      #,(generate-specialized-unify-body v t2 #'st no-occur?)))

(define/hygienic (generate-== stx) #:expression
  (define no-occur? (syntax-property stx SKIP-CHECK))

  (if (specialize-unify?)
    (syntax-parse stx
      #:literal-sets (mk-literals)
      #:literals (quote cons)
      [(== (~and t1 (#%lv-ref v:id)) t2)
       (generate-specialized-unify #'v #'t2 no-occur?)]
      [(== (~and t1 (term-from-expression e)) (~and t2 (~not (#%lv-ref _))))
       (generate-specialized-unify #'(check-and-unseal-vars-in-term e #'e) #'t2 no-occur?)]
      [_ (error 'generate-== "invariant violation")])
    (generate-plain-== stx no-occur?)))

(define/hygienic (generate-plain-== stx no-occur?) #:expression
  (syntax-parse stx
    [(== t1 t2)
     #`(lambda (st)
         #,(generate-runtime-unify (generate-term #'t1) (generate-term #'t2) #'st no-occur?))]))

(define/hygienic (generate-term stx) #:expression
  (syntax-parse stx
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%lv-ref v:id) #'v]
    [(term-from-expression e)
     #'(check-and-unseal-vars-in-term e #'e)]
    [(quote d)
     #'(quote d)]
    [(cons t1:term/c t2:term/c)
     #`(cons #,(generate-term #'t1) #,(generate-term #'t2))]))

(module* test racket/base
  (require (except-in rackunit fail))

  (require (prefix-in mku: "../../mk/private-unstable.rkt")
           "../runtime.rkt")
  (require (for-syntax "test/unit-test-progs.rkt"
                       "prop-vars.rkt")
           "test/unit-test-progs.rkt")

  (require (for-template "../forms.rkt")
           "../forms.rkt"
           "../syntax-classes.rkt")

  (require (for-syntax racket/base (submod "..")))

  ;; A "block" with no unifications; base case.
  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder a))
             (fresh ((~binder x))
               (symbolo (#%lv-ref x))))))
   (generate-prog
    (lambda (st)
      (mku:fresh (x)
                 (lambda (st)
                   ((mku:symbolo x)
                    st))))))


  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder a))
             (fresh ((~binder x))
               (== (#%lv-ref x) (quote 5))))))
   (generate-prog
    (lambda (a)
      (mku:fresh ()
                 (lambda (st)
                   (let ([sc (get-state-from-scope st)])
                     (let ()
                       (let ((v '5))
                         (and st st)))))))))

  #;(begin-for-syntax
    (require racket/pretty)
    
    (pretty-print
     (syntax->datum
     (generate-relation
      (generate-prog
       (ir-rel ((~binder a))
               (fresh (b c)
                 (== (#%lv-ref a) (cons (#%lv-ref b) (cons (#%lv-ref c) (quote ())))))))))))
            
  
  #;(core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder a))
             (fresh ((~binder x))
               (== (#%lv-ref x) (quote 5))))))
   (generate-prog
    (== (#%lv-ref x) (quote 5))))
  
  #;(begin
  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder a))
             (conj
              (== (#%lv-ref a) (quote 7))
              (== (quote 8) (#%lv-ref a))))))
   (generate-prog
     (lambda (a26)
       (mku:conj
         (lambda (st)
            (let-values (((S) (mku:state-S st)))
              (let-values (((v^) (mku:walk a26 S)))
                (let-values (((S^ added)
                              (let-values (((t) (quote 7)))
                                (cond
                                  ((equal? v^ t) (values S (quote ())))
                                  ((mku:var? v^) (mku:ext-s-no-check v^ t S))
                                  (else (values (quote #f) (quote #f)))))))
                  (check-constraints S^ added st)))))
         (mku:== (quote 8) a26)))))

  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binders q a))
             (conj
              (== (#%lv-ref q) (quote 5))
              (== (#%lv-ref q) (#%lv-ref a))))))
   (generate-prog
     (λ (q a)
       (mku:conj
         (lambda (st)
           (let-values (((S) (mku:state-S st)))
             (let-values (((v^) (mku:walk q S)))
               (let-values (((S^ added)
                             (let-values (((t) (quote 5)))
                               (cond
                                 ((equal? v^ t) (values S (quote ())))
                                 ((mku:var? v^) (mku:ext-s-no-check v^ t S))
                                 (else (values (quote #f) (quote #f)))))))
                 (check-constraints S^ added st)))))
        (lambda (st)
          (let-values (((S) (mku:state-S st)))
            (let-values (((v^) (mku:walk q S)))
              (let-values (((S^ added)
                            (let-values (((w^) (mku:walk a S)))
                              (mku:unify v^ w^ S))))
                (check-constraints S^ added st)))))))))

  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder q))
       (fresh ((~binder a))
         (== (#%lv-ref a) (#%lv-ref q))))))
   (generate-prog
     (lambda (q)
       (mku:fresh (a)
         (lambda (st)
           (let ((S (mku:state-S st)))
             (let ((v^ (mku:walk a S)))
               (let-values (((S^ added)
                             (let ((w^ (mku:walk q S)))
                               (mku:unify v^ w^ S))))
                 (check-constraints S^ added st)))))))))

  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder q))
       (fresh ((~binder a))
         (== (#%lv-ref q) (~prop (#%lv-ref a) FIRST-REF #t))))))
   (generate-prog
     (lambda (q)
       (mku:fresh (a)
         (lambda (st)
           (let ((S (mku:state-S st)))
             (let ((v^ (mku:walk q S)))
               (let-values (((S^ added) (mku:ext-s-no-check a v^ S)))
                 (check-constraints S^ added st)))))))))

  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder q))
       (fresh ((~binder a))
         (~prop (== (#%lv-ref q) (#%lv-ref q)) SKIP-CHECK #t)))))
   (generate-prog
     (lambda (q)
       (mku:fresh (a)
         (lambda (st)
           (let ((S (mku:state-S st)))
             (let ((v^ (mku:walk q S)))
               (let-values (((S^ added)
                 (let ((w^ (mku:walk q S)))
                   (mku:unify-no-occur-check v^ w^ S))))
                 (check-constraints S^ added st)))))))))

  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binder q))
        (== (#%lv-ref q) (quote 5)))))
   (generate-prog
     (λ (q)
       (λ (st)
         (let ([S (mku:state-S st)])
           (let ([v^ (mku:walk q S)])
             (let-values ([(S^ added)
                           (let ((t (quote 5)))
                             (cond
                               ((equal? v^ t) (values S (quote ())))
                               ((mku:var? v^) (mku:ext-s-no-check v^ t S))
                               (else (values (quote #f) (quote #f)))))])
               (check-constraints S^ added st)))))))))




  )
