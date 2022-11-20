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

(provide generate-relation
         generate-run
         compiled-names)

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

(define compiled-names (make-free-id-table))

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
    [(success) #'(mk:== '2 '2)]
    [(failure) #'(mk:== '2 '3)]
    [(c:unary-constraint t)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-term #'t))]
    [(== t1 t2)
     (generate-== stx)]
    [(c:binary-constraint t1 t2)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-term #'t1) #,(generate-term #'t2))]
    [(#%rel-app n:id t ...)
     (def/stx n^ (free-id-table-ref compiled-names #'n))
     #`(n^ #,@ (stx-map generate-term #'(t ...)))]
    [(disj g1 g2)
     #`(mk:conde
         #,@(stx-map (compose list generate-goal) (collect-disjs this-syntax)))]
        ;; [#,(generate-goal #'g1)]
        ;; [#,(generate-goal #'g2)])]
    [(conj g1 g2)
     #`(mku:conj #,(generate-goal #'g1)
                 #,(generate-goal #'g2))]
    [(fresh (x:id ...) g)
     (compile-block (attribute x) #'g)
     #;#`(mk:fresh (x ...) #,(generate-goal #'g))]
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

(define (split-vars vars ==s)
  (define refed-vars
    (apply free-id-set-union
           (for/list ([==-stx ==s])
             (==-vars ==-stx))))
  
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

(define (compile-block fresh-vars g)
  #;(pretty-print (syntax->datum g))
  (define-values (==s rest-g) (split-block g))

  (define res
    (cond
      [(null? ==s)
       #`(mku:fresh (#,@fresh-vars)
           #,(generate-goal rest-g))]
      [else
  
       (define-values (==-vars other-vars) (split-vars fresh-vars ==s))

     
       #`(mku:fresh (#,@(free-id-set->list other-vars))
                    (lambda (st)
                      #,(compile-==-rec
                         ==s ==-vars #'st
                         (lambda (st)
                           (if rest-g
                               #`(#,(generate-goal rest-g)
                                  #,st)
                               st)))))]))
    #;(pretty-print (syntax->datum res))
  res)

(define (compile-==-rec ==s remaining-==-vars st k)
  (cond
    [(null? ==s)
     (unless (free-id-set-empty? remaining-==-vars)
       (error 'compile-block "should be no remaining-==-vars here"))
     (k st)]
    [else
     (generate-==-rec
      (car ==s)
      remaining-==-vars
      st
      (lambda (remaining-==-vars^ st^)
        #`(and #,st^
               #,(compile-==-rec (cdr ==s) remaining-==-vars^ st^ k))))]))

(define (fresh-term-vars t all-fresh-vars)
  (free-id-set-intersect all-fresh-vars (term-vars t)))

(define (generate-==-rec stx ==-vars st k)
  (define no-occur? (syntax-property stx SKIP-CHECK))
  (syntax-parse stx
    #:context 'generate-==-rec
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(== (#%lv-ref v:id) t2)
     (define t2-fresh-vars (fresh-term-vars #'t2 ==-vars))
   
     (cond
       [(and (free-id-set-member? ==-vars #'v)
             (not (free-id-set-member? t2-fresh-vars #'v)))
        (define/syntax-parse (t2-var ...) (free-id-set->list t2-fresh-vars))
        (define ==-vars^ (free-id-set-remove
                          (free-id-set-subtract ==-vars t2-fresh-vars)
                          #'v))
        #`(let ([sc (mku:subst-scope (mku:state-S st))])
            (let ([t2-var (mku:var sc)] ...)
              (let ([v #,(generate-term #'t2)])
                #,(k ==-vars^ st))))]
       [else
        (define t-vars (free-id-set-union
                        (free-id-set-intersect ==-vars (immutable-free-id-set (list #'v)))
                        t2-fresh-vars))
        (define/syntax-parse (t-var ...) (free-id-set->list t-vars))
        (define ==-vars^ (free-id-set-subtract ==-vars t-vars))
        #`(let ([sc (mku:subst-scope (mku:state-S #,st))])
            (let ([t-var (mku:var sc)] ...)
              (let ([st^ #,(generate-specialized-unify-body #'v #'t2 st no-occur?)])
                #,(k ==-vars^ #'st^))))])]
    
    [(== (rkt-term e) (~and t2 (~not (#%lv-ref _))))
     (define t2-fresh-vars (fresh-term-vars #'t2 ==-vars))
     (define/syntax-parse (t2-var ...) (free-id-set->list t2-fresh-vars))
     (define ==-vars^ (free-id-set-subtract ==-vars t2-fresh-vars))
     #`(let ([sc (mku:subst-scope (mku:state-S #,st))])
         (let ([t2-var (mku:var sc)] ...)
           (let ([st^ #,(generate-specialized-unify-body #'v #'t2 st no-occur?)])
             #,(k ==-vars^ #'st^))))]
    [(== t1 t2)
     (error 'generate-==-rec "invariant violation")]))



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
  (syntax-parse stx
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(== (~and t1 (#%lv-ref v:id)) t2)
     (generate-specialized-unify #'v #'t2 no-occur?)]
    [(== (~and t1 (rkt-term e)) (~and t2 (~not (#%lv-ref _))))
     (generate-specialized-unify #'(check-term e #'e) #'t2 no-occur?)]
    ;; We should not see this case, but if we do, we don’t know squat.
    [(== t1 t2)
     #`(mku:== #,(generate-term #'t1) #,(generate-term #'t2))]))

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
               (check-constraints S^ added st))))))))




  )
