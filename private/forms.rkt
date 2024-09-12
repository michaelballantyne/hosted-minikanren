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
 apply-relation
 term-from-expression
 goal-from-expression

 succeed
 fail

 ir-rel

 (for-syntax mk-literals))

(require
  syntax-spec-v2/private/ee-lib/define
  "spec.rkt"
  (for-syntax
   racket/base
   syntax/parse))

(begin-for-syntax
  (define-literal-set mk-literals
    #:literal-sets (term-literals goal-literals)
    ()))


