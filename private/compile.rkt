#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 (for-template "runtime.rkt")
 (for-template racket/base)
 (for-template (prefix-in mk: minikanren))
 (only-in syntax/parse [define/syntax-parse def/stx])
 "syntax-classes.rkt"
 "env-rep.rkt"
 (for-template "forms.rkt"))

(provide
 build-conj
 reorder-conjunction
 reorder-conjunctions
 generate-code
 generate-relation
 compiled-names)



; Optimization pass

(define (build-conj l)
  (when (null? l) (error 'build-conj "requires at least one item"))
  (let recur ([l (reverse l)])
    (if (= (length l) 1)
        (car l)
        #`(conj
           #,(recur (cdr l))
           #,(car l)))))

(define (reorder-conjunction stx)
  (define lvars '())
  (define constraints '())
  (define others '())
  (let recur ([stx stx])
    (syntax-parse stx #:literals (conj fresh ==)
                  [(conj g1 g2) (recur #'g1) (recur #'g2)]
                  [(fresh (x:id ...) g)
                   (set! lvars (cons (syntax->list #'(x ...)) lvars))
                   (recur #'g)]
                  [(~or (c:unary-constraint t)
                        (c:binary-constraint t1 t2))
                   (set! constraints (cons this-syntax constraints))]
                  [_ (set! others (cons (reorder-conjunctions this-syntax) others))]))
  (let ([lvars (apply append (reverse lvars))]
        [body (build-conj (append (reverse constraints) (reverse others)))])
    (if (null? lvars)
        body
        #`(fresh #,lvars #,body))))

(define (reorder-conjunctions stx)
  (define (maybe-reorder stx)
    (syntax-parse stx
      #:literals (conj fresh)
      [((~or conj fresh) . _) (reorder-conjunction this-syntax)]
      [_ this-syntax]))
  (map-transform maybe-reorder stx))

(define compiled-names (make-free-id-table))

(define constraint-impls
  (make-free-id-table
   (hash #'symbolo #'mk:symbolo
         #'stringo #'mk:stringo
         #'numbero #'mk:numbero
         #'== #'mk:==
         #'=/= #'mk:=/=
         #'absento #'mk:absento)))

; Code generation

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

