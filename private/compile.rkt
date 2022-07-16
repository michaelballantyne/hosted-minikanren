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
 "syntax-classes.rkt"
 "env-rep.rkt"
 "compile/generate-code.rkt"
 "compile/reorder-conj.rkt"
 "compile/fold.rkt"
 "compile/first-refs.rkt"
 (for-template "forms.rkt"))

(provide
 compiled-names
 compile-run
 compile-relation)


(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    [(~or (run _ (_ ...) _)
          (run* (_ ...) _))
     (define folded (fold/run this-syntax))
     (define reordered (reorder-conj/run folded))
     (define first-annots (first-refs/run reordered))
     (generate-run first-annots)]))

(define/hygienic (compile-relation stx) #:expression
  (syntax-parse stx
    [(ir-rel (x ...) g)
     (define folded (fold/rel this-syntax))
     (define reordered (reorder-conj/rel folded))
     (define first-annots (first-refs/rel reordered))
     (generate-relation first-annots)]))

