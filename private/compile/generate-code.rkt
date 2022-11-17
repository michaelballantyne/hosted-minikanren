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
     #`(mk:conj #,(generate-goal #'g1)
                #,(generate-goal #'g2))]
    [(fresh (x:id ...) g)
     #`(mk:fresh (x ...) #,(generate-goal #'g))]
    [(apply-relation e t ...)
     #`((relation-value-proc (check-relation e #'e))
        #,@(stx-map generate-term #'(t ...)))]))

;; stx stx stx -> stx
(define/hygienic (generate-specialized-unify-body v^ t2 S no-occur?) #:expression
  (syntax-parse t2
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%lv-ref w:id)
     (let ([first-ref? (syntax-property t2 FIRST-REF)])
       (cond
         [first-ref? #`(mku:ext-s-no-check w #,v^ #,S)]
         [no-occur?
          #`(let ([w^ (mku:walk w #,S)])
              (mku:unify-no-occur-check #,v^ w^ #,S))]
         [else
          #`(let ([w^ (mku:walk w #,S)])
              (mku:unify #,v^ w^ #,S))]))]
    [(rkt-term e)
     (if no-occur?
         #`(mku:unify-no-occur-check #,v^ (check-term e #'e) #,S)
         #`(mku:unify #,v^ (check-term e #'e) #,S))]
    [(#%term-datum (~or l:number l:string l:boolean))
     #`(let ([t (quote l)])
         (cond
           [(equal? #,v^ t) (values #,S '())]
           [(mku:var? #,v^) (values (mku:subst-add #,S #,v^ t) (list (cons #,v^ t)))]
           [else (values #f #f)]))]
    [(quote l)
     #`(let ([t (quote l)])
         (cond
           [(equal? #,v^ t) (values #,S '())]
           [(mku:var? #,v^) (values (mku:subst-add #,S #,v^ t) (list (cons #,v^ t)))]
           [else (values #f #f)]))]
    [(cons t2-a:term/c t2-b:term/c)
     #`(cond
         [(mku:var? #,v^)
          (let ([t #,(generate-term t2)])
            (values (mku:subst-add #,S #,v^ t) (list (cons #,v^ t))))]
         [(pair? #,v^)
          (let ([v^-a-walked (mku:walk (car #,v^) #,S)])
            (let-values ([(S^ added-car) #,(generate-specialized-unify-body #'v^-a-walked #'t2-a S no-occur?)])
              (if S^
                  (let ([v^-d-walked (mku:walk (cdr #,v^) S^)])
                    (let-values ([(S^ added-cdr) #,(generate-specialized-unify-body #'v^-d-walked #'t2-b #'S^ no-occur?)])
                      (values S^ (append added-car added-cdr))))
                  (values #f #f))))]
         [else (values #f #f)])]))

;; stx stx -> stx
(define/hygienic (generate-specialized-unify v t2 no-occur?) #:expression
  #`(λ (st)
      (let ([S (mku:state-S st)])
        (let ([v^ (mku:walk #,v S)])
          (let-values ([(S^ added) #,(generate-specialized-unify-body #'v^ t2 #'S no-occur?)])
            (check-constraints S^ added st))))))

;; stx stx -> stx
(define/hygienic (generate-specialized-unify-rkt-term v t2 no-occur?) #:expression
  #`(λ (st)
      (let ([S (mku:state-S st)])
        (let-values ([(S^ added) #,(generate-specialized-unify-body #'v t2 #'S no-occur?)])
          (check-constraints S^ added st)))))

(define/hygienic (generate-== stx) #:expression
  (define no-occur? (syntax-property stx SKIP-CHECK))
  (syntax-parse stx
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(== (~and t1 (#%lv-ref v:id)) t2)
     (generate-specialized-unify #'v #'t2 no-occur?)]
    [(== (~and t1 (rkt-term e)) (~and t2 (~not (#%lv-ref _))))
     (generate-specialized-unify #'e #'t2 no-occur?)]
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
    [(#%term-datum l:number)
     #'(quote l)]
    [(#%term-datum l:string)
     #'(quote l)]
    [(#%term-datum l:boolean)
     #'(quote l)]
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
              (== (#%lv-ref a) (#%term-datum 7))
              (== (#%term-datum 8) (#%lv-ref a))))))
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
                                  ((mku:var? v^) (values (mku:subst-add S v^ t) (list (cons v^ t))))
                                  (else (values (quote #f) (quote #f)))))))
                  (check-constraints S^ added st)))))
         (mku:== (quote 8) a26)))))

  (core-progs-equal?
   (generate-relation
    (generate-prog
     (ir-rel ((~binders q a))
             (conj
              (== (#%lv-ref q) (#%term-datum 5))
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
                                 ((mku:var? v^) (values (mku:subst-add S v^ t) (list (cons v^ t))))
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
        (== (#%lv-ref q) (#%term-datum 5)))))
   (generate-prog
     (λ (q)
       (λ (st)
         (let ([S (mku:state-S st)])
           (let ([v^ (mku:walk q S)])
             (let-values ([(S^ added)
                           (let ((t (quote 5)))
                             (cond
                               ((equal? v^ t) (values S (quote ())))
                               ((mku:var? v^) (values (mku:subst-add S v^ t) (list (cons v^ t))))
                               (else (values (quote #f) (quote #f)))))])
               (check-constraints S^ added st))))))))




  )
