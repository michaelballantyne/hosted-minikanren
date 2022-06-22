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
 (for-template "forms.rkt"))

(provide
 compiled-names
 compile-run
 compile-relation)


(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    [(~or (run _ (_ ...) _)
          (run* (_ ...) _))
     (define reordered (reorder-conj/run this-syntax))
     (generate-run reordered)]))

(define/hygienic (compile-relation stx) #:expression
  (syntax-parse stx
    [(ir-rel (x ...) g)
     (define reordered (reorder-conj/rel this-syntax))
     (generate-relation reordered)]))

