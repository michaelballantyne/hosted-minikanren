#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 racket/math
 (for-template "runtime.rkt")
 (for-template racket/base)
 (for-template (prefix-in mk: minikanren))
 (only-in syntax/parse [define/syntax-parse def/stx])
 "syntax-classes.rkt"
 "env-rep.rkt"
 (for-template "forms.rkt")

 ; Compiler Passes
 "compile/reorder-conj.rkt")

(provide
 compiled-names
 compile-run
 compile-relation)

; Code generation

(define compiled-names (make-free-id-table))

(define constraint-impls
  (make-free-id-table
   (hash #'symbolo #'mk:symbolo
         #'stringo #'mk:stringo
         #'numbero #'mk:numbero
         #'== #'mk:==
         #'=/= #'mk:=/=
         #'absento #'mk:absento)))

(define/hygienic (generate-code stx) #:expression
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
     #`(cons #,(generate-code #'t1) #,(generate-code #'t2))]
    
    ; core goals
    [(c:unary-constraint t)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-code #'t))]
    [(c:binary-constraint t1 t2)
     (def/stx c^ (free-id-table-ref constraint-impls #'c))
     #`(c^ #,(generate-code #'t1) #,(generate-code #'t2))]
    [(#%rel-app n:id t ...)
     (def/stx n^ (free-id-table-ref compiled-names #'n))
     #`((relation-value-proc n^) #,@ (stx-map generate-code #'(t ...)))]
    [(disj g1 g2)
     #`(mk:conde
        [#,(generate-code #'g1)]
        [#,(generate-code #'g2)])]
    [(conj g1 g2)
     #`(mk:fresh ()
                 #,(generate-code #'g1)
                 #,(generate-code #'g2))]
    [(fresh (x:id ...) g)
     #`(mk:fresh (x ...) #,(generate-code #'g))]
    [(apply-relation e t ...)
     #`((relation-value-proc (check-relation e #'e))
        #,@(stx-map generate-code #'(t ...)))]
    
    ))

(define/hygienic (generate-relation stx) #:expression
  (syntax-parse stx
    [(_ (x^ ...) g^)
     #`(relation-value (lambda (x^ ...) #,(generate-code #'g^)))]))

(define/hygienic (compile-run query) #:expression
  (syntax-parse query
    [(run n (q ...) g)
     (define reordered (reorder-conj/run #'g))
     (define compiled (generate-code reordered))
     #`(mk:run (check-natural n #'n) (q ...) #,compiled)]
    [(run* (q ...) g)
     (define reordered (reorder-conj/run #'g))
     (define compiled (generate-code reordered))
     #`(mk:run* (q ...) #,compiled)]))

(define/hygienic (compile-relation rel) #:expression
  (syntax-parse rel
    [(relation (x ...) g)
     (define reordered (reorder-conj/rel this-syntax))
     (generate-relation reordered)]))

