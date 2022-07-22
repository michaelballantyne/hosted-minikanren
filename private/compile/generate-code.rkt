#lang racket/base

(require (for-template (prefix-in mk: "../../mk/main.rkt")
                       racket/base
                       "../forms.rkt"
                       "../runtime.rkt")
         ee-lib
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
     #`(relation-value (lambda (x^ ...) #,(generate-goal #'g^)))]))

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
    [(success) #'(== '2 '2)]
    [(failure) #'(== '2 '3)]
    [(c:unary-constraint t)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-term #'t))]
    [(c:binary-constraint t1 t2)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-term #'t1) #,(generate-term #'t2))]
    [(#%rel-app n:id t ...)
     (def/stx n^ (free-id-table-ref compiled-names #'n))
     #`((relation-value-proc n^) #,@ (stx-map generate-term #'(t ...)))]
    [(disj g1 g2)
     #`(mk:conde
         ;; #,@(stx-map (compose list generate-goal) (collect-disjs this-syntax)))]
        [#,(generate-goal #'g1)]
        [#,(generate-goal #'g2)])]
    [(conj g1 g2)
     #`(mk:tmp-bind* #,(generate-goal #'g1)
                     #,(generate-goal #'g2))]
    [(fresh (x:id ...) g)
     #`(mk:fresh (x ...) #,(generate-goal #'g))]
    [(apply-relation e t ...)
     #`((relation-value-proc (check-relation e #'e))
        #,@(stx-map generate-term #'(t ...)))]))

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
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
		       (prefix-in mk: "../../mk/main.rkt")
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod ".."))
	             )
  
  #;(core-progs-equal?
      (generate-relation
        (generate-prog
          (ir-rel () (== '5 '5))))
      (generate-prog
        (lambda () (#%app mk:== '5 '5))))
    
  )
