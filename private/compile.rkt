#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 (for-template "runtime.rkt")
 (for-template racket/base)
 (for-template (prefix-in mk: "../mk/mk.rkt"))
 (only-in syntax/parse [define/syntax-parse def/stx])
 (only-in threading ~>)
 "syntax-classes.rkt"
 "env-rep.rkt"
 "compile/generate-code.rkt"
 "compile/reorder-conj.rkt"
 "compile/fold.rkt"
 "compile/first-refs.rkt"
 "compile/remove-noop.rkt"
 "compile/remove-unused-vars.rkt"
 "compile/propagate-fail.rkt"
 (for-template "forms.rkt"))

(provide
 compiled-names
 compile-run
 compile-relation)


(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    [(~or (run _ (_ ...) _)
          (run* (_ ...) _))
     (~> this-syntax
         fold/run
         reorder-conj/run
         remove-noop/run
         remove-unused-vars/run
         propagate-fail/run

         ;; annotation passes, no shape-changing past this point
         first-refs/run
         generate-run)]))

(define/hygienic (compile-relation stx) #:expression
  (syntax-parse stx
    [(ir-rel (x ...) g)
     (~> this-syntax
         fold/rel
         reorder-conj/rel
         remove-noop/rel
         remove-unused-vars/rel
         propagate-fail/rel

         ;; annotation passes, no shape-changing past this point
         first-refs/rel
         generate-relation)]))

