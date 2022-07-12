#lang racket/base

(require
 ee-lib
 syntax/parse
 syntax/stx
 (for-template racket/base)
 (only-in syntax/parse [define/syntax-parse def/stx])
 "syntax-classes.rkt"
 "env-rep.rkt"
 (for-template "forms.rkt"))

(provide
 expand-term
 expand-goal
 expand-relation
 bind-logic-vars!)

; Expander

(define (bind-logic-var! name)
  (bind! name (logic-var-binding-rep)))

(define (bind-logic-vars! names)
  (for/list ([x (syntax->list names)])
    (bind-logic-var! x)))

(define (maybe-interposition form-id ctx-stx)
  (let ([interposition-id (datum->syntax ctx-stx (syntax-e form-id))])
    (if (lookup interposition-id (lambda (v) #t))
        interposition-id
        form-id)))

(define/hygienic (expand-term stx) #:expression
  (syntax-parse stx
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    ; core terms
    [(#%lv-ref v:id)
     (unless (lookup #'v logic-var-binding?)
       (raise-syntax-error #f "unbound logic variable" #'v))
     this-syntax]
    [(~describe "(rkt-term <exp>)" (rkt-term e))
     (qstx/rc (rkt-term #,(local-expand #'e 'expression null)))]
    [(#%term-datum l:number) this-syntax]
    [(#%term-datum l:boolean) this-syntax]
    [(#%term-datum l:string) this-syntax]
    [(~describe "(quote <datum>)" (quote d)) this-syntax]
    [(~describe
      "(cons <term> <term>)"
      (cons t1:term/c t2:term/c))
     (qstx/rc (cons #,(expand-term #'t1) #,(expand-term #'t2)))]
    
    ; term macros
    [(head:id . rest)
     #:do [(define binding (lookup #'head term-macro?))]
     #:when binding
     (expand-term
       (apply-as-transformer (lambda (stx) (term-macro-transform binding stx))
                             #'head
                             'expression
                             stx))]
    
    ; interposition points
    [var:id
     #:when (lookup #'var logic-var-binding?)
     (with-syntax ([#%lv-ref (maybe-interposition #'#%lv-ref this-syntax)])
       (expand-term (qstx/rc (#%lv-ref var))))]
    [var:id
     (with-syntax ([rkt-term (maybe-interposition #'rkt-term this-syntax)])
       (expand-term (qstx/rc (rkt-term var))))]
    [(~or* l:number l:boolean l:string)
     (with-syntax ([#%term-datum (maybe-interposition #'#%term-datum this-syntax)])
       (expand-term (qstx/rc (#%term-datum l))))]
    
    [_ (raise-syntax-error #f "not a term expression" stx)]))

;; INVARIANT: nullary constraints should not be available in the surface syntax for the eDSL, so we don't expand them
(define/hygienic (expand-goal stx) #:expression
  (syntax-parse stx
    #:literal-sets (mk-literals)
    ; core goals
    [(c:unary-constraint t)
     (qstx/rc (c #,(expand-term #'t)))]
    [(c:binary-constraint t1 t2)
     (qstx/rc (c #,(expand-term #'t1) #,(expand-term #'t2)))]
    [(#%rel-app n:id t ...)
     (define binding (lookup #'n relation-binding?))
     (unless binding
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
     #:do [(define binding (lookup #'head goal-macro?))]
     #:when binding
     (expand-goal
       (apply-as-transformer (lambda (stx) (goal-macro-transform binding stx))
                             #'head
                             'expression
                             stx))]

    ; interposition points
    [(head:id . rest)
     #:when (lookup #'head relation-binding?)
     (with-syntax ([#%rel-app (maybe-interposition #'#%rel-app this-syntax)])
       (expand-goal (qstx/rc (#%rel-app head . rest))))]
    
    [_ (raise-syntax-error
        #f
        "not a goal constructor or relation name;\n   expected a relation application or other goal form\n"
        stx)]))

(define/hygienic (expand-relation stx) #:expression
  (syntax-parse stx
    [(relation (x:id ...) g)
     (with-scope sc
       (def/stx (x^ ...) (bind-logic-vars! (add-scope #'(x ...) sc)))
       (def/stx g^ (expand-goal (add-scope #'g sc)))
       (qstx/rc (ir-rel (x^ ...) g^)))]))
  

 
