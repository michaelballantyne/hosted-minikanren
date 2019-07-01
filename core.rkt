#lang racket/base

; This module defines the embedded expander and compiler for the
; core language.
;
; `define-goal-macro` and define-term-macro allow sugar definitions.
;
; The `private` submodule exports the syntax generics necessary
; to extend the core langauge,

(require
  ee-lib/define
  ee-lib/errors
  syntax/parse/define
  (prefix-in mk: minikanren)
  (for-syntax
   syntax/stx
   racket/syntax
   syntax/id-table
   ee-lib
   racket/base
   syntax/parse
   racket/generic
   (only-in syntax/parse [define/syntax-parse def/stx])
   "syntax-classes.rkt"
   ))

(provide run run* relation define-relation
         quote cons #%term-datum #%lv-ref
         absento symbolo numbero =/= ==
         conj disj fresh #%rel-app
         #%rkt-ref apply-relation rkt-term
         define-goal-macro define-term-macro
         mk-value? relation-value?
         relation-code
         
         (for-syntax gen:term-macro gen:goal-macro))

; Runtime

(struct relation-value [proc])

(define (check-relation val blame-stx)
  (if (relation-value? val)
      val
      (raise-argument-error/stx
       'apply-relation
       "relation-value?"
       val
       blame-stx)))

(define (mk-value? v)
  (or (symbol? v)
      (string? v)
      (number? v)
      (null? v)
      (and (pair? v)
           (mk-value? (car v))
           (mk-value? (cdr v)))))

(define (check-term val blame-stx)
  (if (mk-value? val)
      val
      (raise-argument-error/stx 'term "mk-value?" val blame-stx)))

; Syntax

(define-literal-forms mk-literals
  "miniKanren forms cannot be used in a Racket expression context"
  [conj
   disj
   fresh
   ==
   =/=
   absento
   symbolo
   numbero
   #%term-datum
   #%lv-ref
   #%rkt-ref

   #%rel-app
   rkt-term
   apply-relation])

(begin-for-syntax
  ; Interfaces for bindings
  
  (define-generics term-macro
    (term-macro-transform term-macro stx))
  (define-generics goal-macro
    (goal-macro-transform goal-macro stx))
  (define-generics relation-binding
    (relation-argument-count relation-binding))
  (define-generics logic-var-binding)
  
  (struct term-macro-rep [transformer]
    #:methods gen:term-macro
    [(define (term-macro-transform s stx)
       ((term-macro-rep-transformer s) stx))])
  (struct goal-macro-rep [transformer]
    #:methods gen:goal-macro
    [(define (goal-macro-transform s stx)
       ((goal-macro-rep-transformer s) stx))])
  (struct relation-binding-rep [argument-count]
    #:methods gen:relation-binding
    [(define (relation-argument-count s)
       (relation-binding-rep-argument-count s))]
    #:property prop:set!-transformer
    (lambda (stx)
      (raise-syntax-error
       #f
       "relations may only be used in the context of a miniKanren goal"
       stx)))
  (struct logic-var-binding-rep []
    #:methods gen:logic-var-binding []
    #:property prop:set!-transformer
    (lambda (stx)
      (raise-syntax-error
       #f
       (string-append
        "logic variables may only be used in miniKanren terms"
        ", and not across foreign language boundaries")
       stx)))
  
  (define (bind-logic-var! name)
    (bind! name #'(logic-var-binding-rep)))
  (define (bind-logic-vars! names)
    (for/list ([x (syntax->list names)])
      (bind-logic-var! x)))

  ; Expander
  
  (define/hygienic (expand-term stx) #:expression
    (syntax-parse stx
      #:literal-sets (mk-literals)
      #:literals (quote cons)
      ; core terms
      [(#%lv-ref v:id)
       (unless (logic-var-binding? (lookup #'v))
         (raise-syntax-error #f "unbound logic variable" #'v))
       this-syntax]
      [(~describe "(rkt-term <exp>)" (rkt-term e))
       (qstx/rc (rkt-term #,(local-expand #'e 'expression null)))]
      [(#%term-datum l:number) this-syntax]
      [(~describe "(quote <datum>)" (quote d)) this-syntax]
      [(~describe
        "(cons <term> <term>)"
        (cons t1:term/c t2:term/c))
       (qstx/rc (cons #,(expand-term #'t1) #,(expand-term #'t2)))]
      
      ; term macros
      [(head:id . rest)
       #:do [(define binding (lookup #'head))]
       #:when (term-macro? binding)
       (expand-term (term-macro-transform binding stx))]
      
      ; interposition points
      [var:id
       #:when (logic-var-binding? (lookup #'var))
       (with-syntax ([#%lv-ref (datum->syntax stx '#%lv-ref)])
         (expand-term (qstx/rc (#%lv-ref var))))]
      [var:id
       (with-syntax ([rkt-term (datum->syntax stx 'rkt-term)])
         (expand-term (qstx/rc (rkt-term var))))]
      [(~or* l:number l:boolean)
       (with-syntax ([#%term-datum (datum->syntax stx '#%term-datum)])
         (expand-term (qstx/rc (#%term-datum l))))]
      
      [_ (raise-syntax-error #f "not a term expression" stx)]))

  (define-syntax-class unary-constraint
    #:literal-sets (mk-literals)
    (pattern (~or symbolo numbero)))
  (define-syntax-class binary-constraint
    #:literal-sets (mk-literals)
    (pattern (~or == =/= absento)))
  (define-syntax-class binary-goal-constructor
    #:literal-sets (mk-literals)
    (pattern (~or conj disj)))
    
  (define/hygienic (expand-goal stx) #:expression
    (syntax-parse stx
      #:literal-sets (mk-literals)
      ; core goals
      [(c:unary-constraint t)
       (qstx/rc (c #,(expand-term #'t)))]
      [(c:binary-constraint t1 t2)
       (qstx/rc (c #,(expand-term #'t1) #,(expand-term #'t2)))]
      [(#%rel-app n:id t ...)
       (define binding (lookup #'n))
       (unless (relation-binding? binding)
         (raise-syntax-error #f "unbound relation" #'n))
       (let ([expected (relation-argument-count binding)]
             [actual (length (syntax->list #'(t ...)))])
         (unless (= expected actual)
           (raise-syntax-error
            (syntax-e #'n)
            (format "wrong number of arguments to relation. Expected ~a; Given ~a"
                    expected actual)
            this-syntax)))
       (qstx/rc (#%rel-app n #,@(stx-map expand-term #'(t ...))))]
      [(c:binary-goal-constructor g1 g2)
       (qstx/rc (c #,(expand-goal #'g1) #,(expand-goal #'g2)))]
      [(fresh (x:id ...) g)
       (with-scope sc
         (def/stx (x^ ...) (bind-logic-vars! (add-scope #'(x ...) sc)))
         (def/stx g^ (expand-goal (add-scope #'g sc)))
         (qstx/rc (fresh (x^ ...) g^)))]
      [(apply-relation e t ...)
       (def/stx e^ (local-expand #'e 'expression null))
       (qstx/rc (apply-relation e^ #,@(stx-map expand-term #'(t ...))))]
      
      ; goal macros
      [(head:id . rest)
       #:do [(define binding (lookup #'head))]
       #:when (goal-macro? binding)
       (expand-goal (goal-macro-transform binding stx))]

      ; interposition points
      [(head:id . rest)
       #:when (relation-binding? (lookup #'head))
       (with-syntax ([#%rel-app (datum->syntax stx '#%rel-app)])
         (expand-goal (qstx/rc (#%rel-app head . rest))))]
      
      [_ (raise-syntax-error
          #f
          "not a goal constructor or relation name;\n   expected a relation application or other goal form\n"
          stx)]))

  (define/hygienic (expand-relation stx) #:expression
    (syntax-parse stx
      #:literals (relation)
      [(relation (x:id ...) g)
       (with-scope sc
         (def/stx (x^ ...) (bind-logic-vars! (add-scope #'(x ...) sc)))
         (def/stx g^ (expand-goal (add-scope #'g sc)))
         (qstx/rc (relation (x^ ...) g^)))]))
  
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

  
  ; Code generation
  
  (define compiled-names (make-free-id-table))
  
  (define constraint-impls
    (make-free-id-table
     (hash #'symbolo #'mk:symbolo
           #'numbero #'mk:numbero
           #'== #'mk:==
           #'=/= #'mk:=/=
           #'absento #'mk:absento)))

  (define expanded-relation-code (make-free-id-table))

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

  ; Compiler entry point
  (define (compile-goal stx)
    (define expanded (expand-goal stx))
    (define reordered (reorder-conjunctions expanded))
    (define compiled (generate-code reordered))
    compiled)

  )

; run, run*, and define-relation are the interface with Racket

(define-syntax run
  (syntax-parser
    [(~describe
      "(run <number> (<id> ...+) <goal>)"
      (_ n:number b:bindings+/c g:goal/c))
     (ee-lib-boundary
      (with-scope sc
        (def/stx (x^ ...) (bind-logic-vars! (add-scope #'(b.x ...) sc)))
        (def/stx g^ (compile-goal (add-scope #'g sc)))
        #'(mk:run n (x^ ...) g^)))]))

(define-syntax run*
  (syntax-parser
    [(~describe
      "(run* (<id> ...+) <goal>)"
      (_ b:bindings+/c g:goal/c))
     (ee-lib-boundary
      (with-scope sc
        (def/stx (x^ ...) (bind-logic-vars! (add-scope #'(b.x ...) sc)))
        (def/stx g^ (compile-goal (add-scope #'g sc)))
        #'(mk:run* (x^ ...) g^)))]))

(define-syntax relation
  (syntax-parser
    [(~describe
      "(relation (<id> ...) <goal>)"
      (_ b:bindings/c g:goal/c))
     ; some awkwardness to let us capture the expanded and optimized mk code
     (ee-lib-boundary
      (define expanded (expand-relation this-syntax))
      (define reordered (reorder-conjunctions expanded))
      (define name (syntax-property this-syntax 'name))
      (when name
        (free-id-table-set! expanded-relation-code
                            name
                            reordered))
      (syntax-parse reordered
        [(_ (x^ ...) g^)
         #`(relation-value (lambda (x^ ...) #,(generate-code #'g^)))]))]))

(define-syntax define-relation
  (syntax-parser 
    [(~describe
      "(define-relation (<name:id> <arg:id> ...) <goal>)"
      (_ h:define-header/c g:goal/c))
     #`(begin
         ; Bind static information for expansion
         (define-syntax h.name
           (relation-binding-rep #,(length (syntax->list #'(h.v ...)))))
         ; Binding for the the compiled function.
         ; Expansion of `relation` expands and compiles the
         ; body in the definition context's second pass.
         (define tmp #,(syntax-property
                        #'(relation (h.v ...) g)
                        'name #'h.name))
         (begin-for-syntax
           (free-id-table-set! compiled-names #'h.name #'tmp)))]))

(define-syntax-rule
  (define-goal-macro m f)
  (define-syntax m (goal-macro-rep f)))

(define-syntax-rule
  (define-term-macro m f)
  (define-syntax m (term-macro-rep f)))

(define-syntax relation-code
  (syntax-parser
    [(_ name)
     (if (eq? 'expression (syntax-local-context))
         (ee-lib-boundary
          (when (not (relation-binding? (lookup #'name)))
            (raise-syntax-error #f "not bound to a relation" #'name))
          (define code (free-id-table-ref expanded-relation-code #'name #f))
          (when (not code)
            (error 'relation-code "can only access code of relations defined in the current module"))
          #`#'#,code)
         #'(#%expression (relation-code name)))]))
