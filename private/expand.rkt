#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 (only-in syntax/parse [define/syntax-parse def/stx])
 "syntax-classes.rkt"
 "env-rep.rkt"
 (for-template "forms.rkt"))

(provide
 ;; expand-term
 ;; expand-goal
 ;; expand-relation
 bind-logic-vars!)

  ; Expander
  
  (define (bind-logic-var! name)
    (bind! name (logic-var-binding-rep)))

  (define (bind-logic-vars! names)
    (for/list ([x (syntax->list names)])
      (bind-logic-var! x)))
