#lang racket/base

(require
 ee-lib
 syntax/parse
 (for-template racket/base)
 (only-in syntax/parse [define/syntax-parse def/stx])
 "syntax-classes.rkt"
 "env-rep.rkt"
 (for-template "forms.rkt"))

(provide
 expand-term
 ;; expand-goal
 ;; expand-relation
 bind-logic-vars!)

  ; Expander
  
  (define (bind-logic-var! name)
    (bind! name (logic-var-binding-rep)))

  (define (bind-logic-vars! names)
    (for/list ([x (syntax->list names)])
      (bind-logic-var! x)))

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
       (expand-term (term-macro-transform binding stx))]
      
      ; interposition points
      [var:id
       #:when (lookup #'var logic-var-binding?)
       (with-syntax ([#%lv-ref (datum->syntax stx '#%lv-ref)])
         (expand-term (qstx/rc (#%lv-ref var))))]
      [var:id
       (with-syntax ([rkt-term (datum->syntax stx 'rkt-term)])
         (expand-term (qstx/rc (rkt-term var))))]
      [(~or* l:number l:boolean l:string)
       (with-syntax ([#%term-datum (datum->syntax stx '#%term-datum)])
         (expand-term (qstx/rc (#%term-datum l))))]
      
      [_ (raise-syntax-error #f "not a term expression" stx)]))
