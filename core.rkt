#lang racket/base

; This module defines the embedded expander and compiler for the
; core language.
;
; `define-goal-macro` and define-term-macro allow sugar definitions.
;
; The `private` submodule exports the syntax generics necessary
; to extend the core langauge,

(require
  syntax-generic2/define
  syntax-generic2/errors
  syntax/parse/define
  (prefix-in mk: minikanren)
  (only-in racket/base [quote mk:quote] [#%app mk:app])
  (for-syntax
   racket/syntax
   syntax-generic2
   racket/base
   syntax/parse
   (only-in syntax/parse [define/syntax-parse def/stx])
   "syntax-classes.rkt"
   ))

(provide define-relation relation #%rkt-ref
         (rename-out [disj2 disj] [conj2 conj]
                     [fresh1 fresh] [run-core run])
         (rename-out [new-quote quote] [new-cons cons])
         == #%term-datum #%lv-ref
         absento symbolo numbero =/=
         #%rel-app rkt-term
         define-goal-macro define-term-macro)

; term and goal nonterminals. To start we'll just have core forms.

(struct relation-value [proc])

(begin-for-syntax
  (define-syntax-generic core-term)
  (define-syntax-generic core-goal)
  (define-syntax-generic compile)
  (define-syntax-generic term-macro)
  (define-syntax-generic goal-macro)
  (define-syntax-generic relation-binding-argc)
  (define-syntax-generic relation-binding-proc)
  (define-syntax-generic logic-var-binding)
  (define-syntax-generic map-transform)

  (define the-logic-var-binding
    (generics
     ; Currently unreachable as there are no Racket subexpressions of mk goals
     [expand (λ (stx)
               (raise-syntax-error
                #f
                (string-append
                 "logic variables may only be used in miniKanren terms"
                 ", and not across foreign language boundaries")
                stx))]
     [logic-var-binding (lambda (stx) stx)]))
  
  (define (bind-logic-var! ctx name)
    (bind! ctx name #'the-logic-var-binding))

  (define (make-relation-binding proc n)
    (generics
     [expand (λ (stx) (raise-syntax-error
                       #f "relations may only be used in the context of a miniKanren goal" stx))]
     [relation-binding-argc (lambda (_) n)]
     [relation-binding-proc (lambda (_) proc)]))

  (define (expand-term stx sc)
    (syntax-parse stx
      [var:id
       #:when (logic-var-binding? #'var)
       (with-syntax ([#%lv-ref (datum->syntax stx '#%lv-ref)])
         (expand-term (qstx/rc (#%lv-ref var)) sc))]
      [_ #:when (core-term? stx sc)
         (apply-as-transformer core-term 'expression sc stx)]  ; dispatch to other core term forms
      [_ #:when (term-macro? stx sc)
         (expand-term (apply-as-transformer term-macro 'expression sc stx) sc)]
      [var:id
       (with-syntax ([#%rkt-ref (datum->syntax stx '#%rkt-ref)])
         (expand-term (qstx/rc (#%rkt-ref var)) sc))]
      [(~or* l:number l:boolean)
       (with-syntax ([#%term-datum (datum->syntax stx '#%term-datum)])
         (expand-term (qstx/rc (#%term-datum l)) sc))]
      [_ (raise-syntax-error #f "bad term syntax" stx)]))
  
  (define (expand-goal stx sc)
    (syntax-parse stx
      [_ #:when (core-goal? stx sc)
         (apply-as-transformer core-goal 'expression sc stx)]
      [_ #:when (goal-macro? stx sc)
         (expand-goal (apply-as-transformer goal-macro 'expression sc stx) sc)]
      [(head:id . rest) #:when (relation-binding-argc? #'head sc)
                        (with-syntax ([#%rel-app (datum->syntax stx '#%rel-app)])
                          (expand-goal (qstx/rc (#%rel-app head . rest)) sc))]
      [_ (raise-syntax-error
          #f
          "not a goal constructor or relation name;\n   expected a relation application or other goal form\n"
          stx)]))

  (define (dispatch-compile stx)
    (apply-as-transformer compile 'expression #f stx))

  (define (build-conj2 l)
    (when (null? l) (error 'build-conj2 "requires at least one item"))
    (let recur ([l (reverse l)])
      (if (= (length l) 1)
          (car l)
          #`(conj2
             #,(recur (cdr l))
             #,(car l)))))

  ; TODO: also account for =/=, symbolo, numbero, absento
  (define (reorder-conjunction stx)
    (define lvars '())
    (define unifications '())
    (define others '())
    (let recur ([stx stx])
      (syntax-parse stx #:literals (conj2 fresh1 ==)
        [(conj2 g1 g2) (recur #'g1) (recur #'g2)]
        [(fresh1 (x:id ...) g)
         (set! lvars (cons (syntax->list #'(x ...)) lvars))
         (recur #'g)]
        [(== t1 t2) (set! unifications (cons this-syntax unifications))]
        [_ (set! others (cons (reorder-conjunctions this-syntax) others))]))
    (let ([lvars (apply append (reverse lvars))]
          [body (build-conj2 (append (reverse unifications) (reverse others)))])
      (if (null? lvars)
          body
          #`(fresh1 #,lvars #,body))))
  
  (define (reorder-conjunctions stx)
    (define (maybe-reorder stx)
      (syntax-parse stx
        #:literals (conj2 fresh1)
        [((~or conj2 fresh1) . _) (reorder-conjunction this-syntax)]
        [_ this-syntax]))
    (map-transform stx maybe-reorder))

  (define (compile-goal stx sc)
    (define expanded (expand-goal stx sc))
    (define reordered (reorder-conjunctions expanded))
    (define compiled (dispatch-compile reordered))
    compiled)
  )

; run and define-relations are the interface with Racket
; TODO: run*

(define-syntax run-core
  (syntax-parser
    [(~describe
      "(run <number> (<id> ...+) <goal>)"
      (_ n:number b:bindings+/c g:goal/c))
     (with-disappeared-uses-and-bindings
      ; Expansion
      (define ctx (make-def-ctx))
      (define sc (make-scope))
      (def/stx (x^ ...)
        (for/list ([x (syntax->list #'(b.x ...))])
          (bind-logic-var! ctx (add-scope x sc))))
      (def/stx g^ (compile-goal (add-scope #'g sc) ctx))
      #'(mk:run n (x^ ...) g^))]))

(define-syntax relation
  (syntax-parser
    [(~describe
      "(relation (<id> ...) <goal>)"
      (_ b:bindings/c g:goal/c))
     (with-disappeared-uses-and-bindings
      ; Expand
      (define ctx (make-def-ctx))
      (define sc (make-scope))
      (def/stx (x^ ...)
        (for/list ([x (syntax->list #'(b.x ...))])
          (bind-logic-var! ctx (add-scope x sc))))
      (def/stx g^ (compile-goal (add-scope #'g sc) ctx))
      #'(relation-value
         (lambda (x^ ...)
           g^)))]))

(define-syntax define-relation
  (syntax-parser 
    [(~describe
      "(define-relation (<name:id> <arg:id> ...) <goal>)"
      (_ h:define-header/c g:goal/c))
     #`(begin
         ; Bind static information for expansion
         (define-syntax h.name (make-relation-binding #'tmp #,(length (syntax->list #'(h.v ...)))))
         ; Binding for the the compiled function. Expansion of `relation` expands and compiles the
         ; body in the definition context's second pass.
         (define tmp (relation (h.v ...) g)))]))

; Term forms

(define-syntax/generics (#%lv-ref v:id)
  [(core-term)
   (unless (logic-var-binding? #'v)
     (raise-syntax-error #f "unbound logic variable" #'v))
   this-syntax]
  [(compile) #'v]
  [(map-transform f) (f this-syntax)])

(define (mk-value? v)
  (or (symbol? v)
      (string? v)
      (number? v)
      (null? v)
      (and (pair? v)
           (mk-value? (car v))
           (mk-value? (cdr v)))))

(define (check-term-var val blame-stx)
  (if (mk-value? val)
      val
      (raise-argument-error/stx 'term "mk-value?" val blame-stx)))

(define-syntax/generics (#%rkt-ref v)
  [(core-term) #`(#%rkt-ref #,(local-expand #'v 'expression null))]
  [(compile) #'(check-term-var v #'v)]
  [(map-transform f) (f this-syntax)])

(define-syntax/generics (#%term-datum l:number)
  [(core-term) this-syntax]
  [(compile) #'(quote l)]
  [(map-transform f) (f this-syntax)])

(define-syntax/generics (new-quote d)
  [(core-term) this-syntax]
  [(compile) #'(quote d)]
  [(map-transform f) (f this-syntax)])

(define-syntax new-cons
  (generics/parse (~describe
                   "(cons <term> <term>)"
                   (new-cons t1:term/c t2:term/c))
    [(core-term)
     (def/stx t1^ (expand-term #'t1 #f))
     (def/stx t2^ (expand-term #'t2 #f))
     (qstx/rc (new-cons t1^ t2^))]
    [(compile)
     (def/stx t1^ (dispatch-compile #'t1))
     (def/stx t2^ (dispatch-compile #'t2))
     (qstx/rc (cons t1^ t2^))]
    [(map-transform f)
     (f (qstx/rc (new-cons #,(map-transform #'t1 f)
                           #,(map-transform #'t2 f))))]))

; Goal forms

(begin-for-syntax
  (define-syntax-class (binary-term/c name)  
    #:description (format "(~a <term> <term>)" (syntax-e name))
    (pattern (op t1 t2)))
  (define (binary-term-methods name runtime-op)
    (generics/parse (~var p (binary-term/c name))
      [(core-goal)
       (def/stx t1^ (expand-term #'p.t1 #f))
       (def/stx t2^ (expand-term #'p.t2 #f))
       (qstx/rc (p.op t1^ t2^))]
      [(compile)
       (def/stx t1^ (dispatch-compile #'p.t1))
       (def/stx t2^ (dispatch-compile #'p.t2))
       #`(#,runtime-op t1^ t2^)]
      [(map-transform f)
       (def/stx t1^ (map-transform #'p.t1 f))
       (def/stx t2^ (map-transform #'p.t2 f))
       (f (qstx/rc (p.op t1^ t2^)))])))
(define-simple-macro (binary-term name runtime-op)
  (define-syntax name
    (binary-term-methods #'name #'runtime-op)))

(begin-for-syntax
  (define-syntax-class (unary-term/c name)  
    #:description (format "(~a <term>)" (syntax-e name))
    (pattern (op t)))
  (define (unary-term-methods name runtime-op)
    (generics/parse (~var p (unary-term/c name))
      [(core-goal)   
       (def/stx t^ (expand-term #'p.t #f))
       (qstx/rc (p.op t^))]
      [(compile)
       (def/stx t^ (dispatch-compile #'p.t))
       #`(#,runtime-op t^)]
      [(map-transform f)
       (f (qstx/rc (p.op #,(map-transform #'p.t f))))])))
(define-simple-macro (unary-term name runtime-op)
  (define-syntax name
    (unary-term-methods #'name #'runtime-op)))

(binary-term == mk:==)
(binary-term =/= mk:=/=)
(binary-term absento mk:absento)
(unary-term symbolo mk:symbolo)
(unary-term numbero mk:numbero)

(define-syntax/generics (#%rel-app n t ...)
  [(core-goal)
   ; Check relation is bound
   (unless (relation-binding-argc? #'n)
     (raise-syntax-error #f "unbound relation" #'n))
   
   ; Check argument count matches definition
   (let ([expected (relation-binding-argc #'n)]
         [actual (length (syntax->list #'(t ...)))])
     (unless (= expected actual)
       (raise-syntax-error
        (syntax-e #'n)
        (format "wrong number of arguments to relation. Expected ~a; Given ~a" expected actual)
        this-syntax)))
   
   (def/stx (t^ ...)
     (for/list ([t (syntax->list #'(t ...))])
       (expand-term t #f)))
   (qstx/rc (#%rel-app n t^ ...))]
  [(compile)
   (def/stx (t^ ...)
     (for/list ([t (syntax->list #'(t ...))])
       (dispatch-compile t)))
   (def/stx n^ (relation-binding-proc #'n))
   #'(#%app (relation-value-proc n^) t^ ...)]
  [(map-transform f)
   (def/stx (t^ ...)
     (for/list ([t (syntax->list #'(t ...))])
       (map-transform t f)))
   (f (qstx/rc (#%rel-app n t^ ...)))])

(define (check-relation val blame-stx)
  (if (relation-value? val)
      val
      (raise-argument-error/stx
       'apply-relation
       "relation-value?"
       val
       blame-stx)))

(define-syntax/generics (apply-relation e t ...)
  [(core-goal)
   (def/stx e^
     (local-expand #'e 'expression null))
   (def/stx (t^ ...)
     (for/list ([t (syntax->list #'(t ...))])
       (expand-term t #f)))
   (qstx/rc (apply-relation e^ t^ ...))]
  [(compile)
   (def/stx (t^ ...)
     (for/list ([t (syntax->list #'(t ...))])
       (dispatch-compile t)))
   (def/stx e^ #'(check-relation e #'e))
   #'(#%app (relation-value-proc e^) t^ ...)]
  [(map-transform f)
   (def/stx (t^ ...)
     (for/list ([t (syntax->list #'(t ...))])
       (map-transform t f)))
   (f (qstx/rc (apply-relation e t^ ...)))])

(provide apply-relation)

(define-syntax/generics (disj2 g1 g2)
  [(core-goal)
   (def/stx g1^ (expand-goal #'g1 #f))
   (def/stx g2^ (expand-goal #'g2 #f))
   (qstx/rc (disj2 g1^ g2^))]
  [(compile)
   (def/stx g1^ (dispatch-compile #'g1))
   (def/stx g2^ (dispatch-compile #'g2))
   #'(mk:conde [g1^] [g2^])]
  [(map-transform f)
   (f (qstx/rc (disj2 #,(map-transform #'g1 f) #,(map-transform #'g2 f))))])

(define-syntax/generics (conj2 g1 g2)
  [(core-goal)
   (def/stx g1^ (expand-goal #'g1 #f))
   (def/stx g2^ (expand-goal #'g2 #f))
   (qstx/rc (conj2 g1^ g2^))]
  [(compile)
   (def/stx g1^ (dispatch-compile #'g1))
   (def/stx g2^ (dispatch-compile #'g2))
   #'(mk:fresh () g1^ g2^)]
  [(map-transform f)
   (f (qstx/rc (conj2 #,(map-transform #'g1 f) #,(map-transform #'g2 f))))])

(define-syntax/generics (fresh1 (x:id ...) g)
  [(core-goal)
   (define ctx (make-def-ctx))
   (define sc (make-scope))
   (def/stx (x^ ...)
     (for/list ([x (syntax->list #'(x ...))])
       (bind-logic-var! ctx (add-scope x sc))))
   (def/stx g^ (expand-goal (add-scope #'g sc) ctx))
   (qstx/rc (fresh1 (x^ ...) g^))]
  [(compile)
   (def/stx g^ (dispatch-compile #'g))
   #'(mk:fresh (x ...) g^)]
  [(map-transform f)
   (f (qstx/rc (fresh1 (x ...) #,(map-transform #'g f))))])

(define-syntax/generics (rkt-term e)
  [(core-term)
   this-syntax]
  [(compile)
   #'(check-term-var e #'e)]
  [(map-transform f) (f this-syntax)])

(define-syntax-rule
  (define-goal-macro m f)
  (define-syntax m (generics [goal-macro f])))

(define-syntax-rule
  (define-term-macro m f)
  (define-syntax m (generics [term-macro f])))

(module+ private
  (provide
   relation-value-proc
   relation-value?
   mk-value?
   (for-syntax
    core-term
    core-goal
    compile
    map-transform
    expand-term
    dispatch-compile
    )))