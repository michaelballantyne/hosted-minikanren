#lang racket/base
(provide
 (all-defined-out)
 (for-syntax mk-literals))

(require
  ee-lib/define
  (for-syntax
   racket/base))

(define-literal-forms mk-literals
  "miniKanren forms cannot be used in a Racket expression context"
  [conj
   disj
   fresh
   ==
   =/=
   absento
   symbolo
   stringo
   numbero
   #%term-datum
   #%lv-ref
   #%rkt-ref

   #%rel-app
   rkt-term
   apply-relation

   ;; nullary constraints
   success
   failure

   ir-rel])
