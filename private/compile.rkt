#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 racket/math
 (for-template "runtime.rkt")
 (for-template racket/base)
 (for-template (prefix-in mk: minikanren))
 (only-in syntax/parse [define/syntax-parse def/stx])
 "syntax-classes.rkt"
 "env-rep.rkt"
 (for-template "forms.rkt")

 ; Compiler Passes
 "compile/reorder-conj.rkt"
 "compile/generate-code.rkt")

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
    [(relation (x ...) g)
     (define reordered (reorder-conj/rel this-syntax))
     (generate-relation reordered)]))

