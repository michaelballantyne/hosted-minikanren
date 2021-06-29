#lang racket

(provide (all-defined-out))
(require racket/generic
         ee-lib)

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
  (bind! name (logic-var-binding-rep)))

(define (bind-logic-vars! names)
  (for/list ([x (syntax->list names)])
    (bind-logic-var! x)))
