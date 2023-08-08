#lang racket/base
(provide

 conj
 disj
 fresh
 ==
 =/=
 absento
 symbolo
 stringo
 numbero
 #%lv-ref

 #%rel-app
 rkt-term
 apply-relation

 succeed
 fail

 ir-rel
 
 (for-syntax mk-literals))

(require
  ee-lib/define
  "spec.rkt"
  (for-syntax
   racket/base
   syntax/parse))

(begin-for-syntax
  (define-literal-set mk-literals
    #:literal-sets (term-literals goal-literals)
    ()))


