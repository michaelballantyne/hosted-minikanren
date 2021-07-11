#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 racket/math
 (for-template "../runtime.rkt")
 (for-template racket/base)
 (for-template (prefix-in mk: minikanren))
 (only-in syntax/parse [define/syntax-parse def/stx])
 "../syntax-classes.rkt"
 "../env-rep.rkt"
 (for-template "../forms.rkt"))

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

(define/hygienic (generate-goal stx) #:expression
  (syntax-parse stx
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    ; core terms
    [(#%lv-ref v:id)
     #'v]
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
     #`(cons #,(generate-goal #'t1) #,(generate-goal #'t2))]
    
    ; core goals
    [(c:unary-constraint t)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-goal #'t))]
    [(c:binary-constraint t1 t2)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-goal #'t1) #,(generate-goal #'t2))]
    [(#%rel-app n:id t ...)
     (def/stx n^ (free-id-table-ref compiled-names #'n))
     #`((relation-value-proc n^) #,@ (stx-map generate-goal #'(t ...)))]
    [(disj g1 g2)
     #`(mk:conde
        [#,(generate-goal #'g1)]
        [#,(generate-goal #'g2)])]
    [(conj g1 g2)
     #`(mk:fresh ()
                 #,(generate-goal #'g1)
                 #,(generate-goal #'g2))]
    [(fresh (x:id ...) g)
     #`(mk:fresh (x ...) #,(generate-goal #'g))]
    [(apply-relation e t ...)
     #`((relation-value-proc (check-relation e #'e))
        #,@(stx-map generate-goal #'(t ...)))]
    
    ))

(define/hygienic (generate-term stx) #:expression
  #'())


