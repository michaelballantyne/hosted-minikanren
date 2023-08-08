#lang racket/base

(require (for-template (prefix-in mk: "../../mk/main.rkt")
                       racket/base
                       (prefix-in mku: "../../mk/private-unstable.rkt")
                       "../forms.rkt"
                       "../runtime.rkt")
         "prop-vars.rkt"
         ee-lib
         "../forms.rkt"
         "test/unit-test-progs.rkt"
         syntax/id-table
         syntax/id-set
         syntax/parse
         (only-in syntax/parse
                  (define/syntax-parse def/stx))
         syntax/stx
         "../syntax-classes.rkt")

(provide generate-plain/rel
         generate-plain/run
         generate-specialized/rel
         generate-specialized/run

         generate-relation)


(define specialize-unify? (make-parameter #t))

(define (generate-plain/rel stx)
  (parameterize ([specialize-unify? #f])
    (generate-relation stx)))

(define (generate-plain/run stx)
  (parameterize ([specialize-unify? #f])
    (generate-run stx)))

(define (generate-specialized/rel stx)
  (parameterize ([specialize-unify? #t])
    (generate-relation stx)))

(define (generate-specialized/run stx)
  (parameterize ([specialize-unify? #t])
    (generate-run stx)))


(define/hygienic (generate-relation stx) #:expression
  (syntax-parse stx
    [(_ (x^ ...) g^)
     #`(lambda (x^ ...) #,(generate-goal #'g^))]))

(define/hygienic (generate-run stx) #:expression
  (syntax-parse stx
    [(run n (q ...) g)
     #`(mk:run (check-natural n #'n) (q ...) #,(generate-goal #'g))]
    [(run* (q ...) g)
     #`(mk:run* (q ...) #,(generate-goal #'g))]))

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

(define/hygienic (generate-goal stx) #:expression
  (syntax-parse stx
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(success) #'mku:succeed]
    [(failure) #'mku:fail]
    [(c:unary-constraint t)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-term #'t))]
    [(== t1 t2)
     (generate-== stx)]
    [(c:binary-constraint t1 t2)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-term #'t1) #,(generate-term #'t2))]
    [(#%rel-app n:id t ...)
     #`(n #,@ (stx-map generate-term #'(t ...)))]
    [(disj g1 g2)
     #`(mk:conde
         #,@(stx-map (compose list generate-goal) (collect-disjs this-syntax)))]
        ;; [#,(generate-goal #'g1)]
        ;; [#,(generate-goal #'g2)])]
    [(conj g1 g2)
     #`(mku:conj #,(generate-goal #'g1)
                 #,(generate-goal #'g2))]
    [(fresh (x:id ...) g)
     ;(displayln `(specialize block ,(specialize-unify?)))
     (if (specialize-unify?)
       (compile-block #'(x ...) #'g)
       #`(mk:fresh (x ...) #,(generate-goal #'g)))]
    [(apply-relation e t ...)
     #`((relation-value-proc (check-relation e #'e))
        #,@(stx-map generate-term #'(t ...)))]))


(define (take-first-== g)
  (syntax-parse g
    #:context 'take-first-==
    #:literal-sets (mk-literals)
    [(== . _)
     (values this-syntax #f)]
    [(conj g1 g2)
     (define-values (the-== g1-rest) (take-first-== #'g1))
     (if the-==
         (if g1-rest
             (values the-== #`(conj #,g1-rest g2))
             (values the-== #'g2))
         (values #f this-syntax))]
    [_ (values #f this-syntax)]))

(define (split-block g)
  (let loop ([g g] [==s '()])
    (define-values (the-== g-rest) (take-first-== g))
    (if the-==
        (loop g-rest (cons the-== ==s))
        (values (reverse ==s) g))))

(define (split-vars vars unify-gs)
  (define refed-vars
    (for/fold ([ids (immutable-free-id-set)]) ([unify-g unify-gs])
      (free-id-set-union ids (immutable-free-id-set (==-vars unify-g)))))
  
  (values (free-id-set-intersect (immutable-free-id-set vars) refed-vars)
          (free-id-set-subtract (immutable-free-id-set vars) refed-vars)))

(define (term-vars t)
  (syntax-parse t
    #:context 'term-vars
    #:literal-sets (mk-literals)
    #:literals (cons)
    [(#%lv-ref v)
     (immutable-free-id-set (list #'v))]
    [(cons t1 t2)
     (free-id-set-union 
      (term-vars #'t1)
      (term-vars #'t2))]
    [_ (immutable-free-id-set '())]))

(define (==-vars g)
  (syntax-parse g
    #:context '==-vars
    #:literal-sets (mk-literals)
    [(== t1 t2)
     (free-id-set-union (term-vars #'t1) (term-vars #'t2))]))

(require racket/pretty)

(define/hygienic (compile-block fresh-vars g) #:expression
  (define-values (unify-gs rest-g) (split-block g))
  (define-values (unify-vars other-vars) (split-vars (syntax->list fresh-vars) unify-gs))
  
  (define/syntax-parse (other-var ...) (free-id-set->list other-vars))

  #`(mku:fresh (other-var ...)
               (lambda (st)
                 #,(compile-block-conjunction
                    #`(block (#,@unify-gs) #,rest-g) unify-vars #'st))))

(define/hygienic (compile-block-conjunction block remaining-unify-vars st) #:expression
  (syntax-parse block
    [(block () rest-g)
     (unless (free-id-set-empty? remaining-unify-vars)
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
  (immutable-free-id-set
   (for/list ([v (in-free-id-set (term-vars t))]
              #:when (free-id-set-member? all-fresh-vars v))
     v)))

(define/hygienic (generate-let-unify v t2 remaining-unify-vars st rest-block) #:expression
  (define t2-fresh-vars (fresh-term-vars t2 remaining-unify-vars))
  (define remaining-unify-vars^ (free-id-set-remove
                                 (free-id-set-subtract remaining-unify-vars t2-fresh-vars)
                                 v))
  (define/syntax-parse (t2-fresh-var ...) (free-id-set->list t2-fresh-vars))
  #`(let ([sc (mku:subst-scope (mku:state-S #,st))])
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
       [(free-id-set-member? (term-vars #'t2) #'v)
        #'#f
        #;(error 'generate-optimized-unify "invariant violation---occurs check violation should be caught in constant folding")]
       [(free-id-set-member? remaining-unify-vars #'v)
        (generate-let-unify #'v #'t2 remaining-unify-vars st rest-block)]
       [else
        (generate-matching-unify #'v #'t2 remaining-unify-vars st no-occur? rest-block)])]
    [(== (rkt-term e) (~and t2 (~not (#%lv-ref _))))
     #`(let ([v (check-term e #'e)])
         #,(generate-matching-unify #'v #'t2 remaining-unify-vars st no-occur? rest-block))]    
    [(== t1 t2)
     (error 'generate-optimized-unify "invariant violation")]))

(define/hygienic (generate-matching-unify v t2 remaining-unify-vars st no-occur? rest-block) #:expression
  (define t2-fresh-vars (fresh-term-vars t2 remaining-unify-vars))
  (define remaining-unify-vars^ (free-id-set-subtract remaining-unify-vars t2-fresh-vars))
  (define/syntax-parse (body-var ...) (free-id-set->list t2-fresh-vars))
  #`(let ([body (lambda (body-var ... st)
                  #,(continue-block-conjunction rest-block remaining-unify-vars^ #'st))])
      #,(generate-matching-unify-body v t2 st remaining-unify-vars no-occur? #'body #'(body-var ...))))

(define/hygienic (generate-matching-unify-body v t2 st fresh-unify-vars no-occur? join-point-name join-point-vars) #:expression
  (syntax-parse t2
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%lv-ref w:id)
     #:when (free-id-set-member? fresh-unify-vars #'w)
     #`(let ([w #,v]) (#,join-point-name #,@join-point-vars #,st))]
    [(#%lv-ref w:id)
     #:when (syntax-property this-syntax FIRST-REF)
     #`(#,join-point-name #,@join-point-vars (mku:ext-st-no-check w (mku:walk #,v (mku:state-S #,st)) #,st))]
    [(#%lv-ref w:id)
     #`(#,join-point-name #,@join-point-vars #,(generate-runtime-unify v #'w st no-occur?))]
    [(rkt-term e)
     #`(#,join-point-name #,@join-point-vars #,(generate-runtime-unify v #'(check-term e #'e) st no-occur?))]
    [(quote l)
     #`(let ([v^ (mku:walk #,v (mku:state-S #,st))])
         (cond
           [(mku:var? v^) (#,join-point-name #,@join-point-vars (mku:ext-st-check-c v^ 'l #,st))]
           [(equal? v^ 'l) (#,join-point-name #,@join-point-vars #,st)]
           [else #f]))]
    [(cons t2-a:term/c t2-b:term/c)
     
     (define t2-vars (fresh-term-vars t2 fresh-unify-vars))
     (define t2-a-vars (fresh-term-vars #'t2-a fresh-unify-vars))
     (define/syntax-parse (t2-var ...) (free-id-set->list t2-vars))
     (define/syntax-parse (t2-a-var ...) (free-id-set->list t2-a-vars))
     
     #`(let ([v^ (mku:walk #,v (mku:state-S #,st))])
         (cond
           [(mku:var? v^) (let ([t2-var (mku:var (mku:subst-scope (mku:state-S #,st)))] ...)
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
         [first-ref? #`(let ([v^ (mku:walk #,v (mku:state-S #,st))])
                         (mku:ext-st-no-check w v^ #,st))]
         [no-occur?
          #`(mku:unify2-no-occur-check #,v w #,st)]
         [else
          #`(mku:unify2 #,v w #,st)]))]
    [(rkt-term e)
     (if no-occur?
         #`(mku:unify2-no-occur-check #,v (check-term e #'e) #,st)
         #`(mku:unify2 #,v (check-term e #'e) #,st))]
    [(quote l)
     #`(let ([v^ (mku:walk #,v (mku:state-S #,st))])
         (let ([t (quote l)])
         (cond
             [(equal? v^ t) #,st]
             [(mku:var? v^) (mku:ext-st-check-c v^ t #,st)]
             [else #f])))]
    [(cons t2-a:term/c t2-b:term/c)
     #`(let ([v^ (mku:walk #,v (mku:state-S #,st))])
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

  ;(displayln `(specialize == ,(specialize-unify?)))

  (if (specialize-unify?)
    (syntax-parse stx
      #:literal-sets (mk-literals)
      #:literals (quote cons)
      [(== (~and t1 (#%lv-ref v:id)) t2)
       (generate-specialized-unify #'v #'t2 no-occur?)]
      [(== (~and t1 (rkt-term e)) (~and t2 (~not (#%lv-ref _))))
       (generate-specialized-unify #'(check-term e #'e) #'t2 no-occur?)]
      ;; This could arise if we unify two rkt-terms. No way to specialize.
      [_
       (generate-plain-== stx no-occur?)])
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
    [(rkt-term e)
     #'(check-term e #'e)]
    [(quote d)
     #'(quote d)]
    [(cons t1:term/c t2:term/c)
     #`(cons #,(generate-term #'t1) #,(generate-term #'t2))]))

(module* test racket/base
  (require rackunit)

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
                   (let ([sc (mku:subst-scope (mku:state-S st))])
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
