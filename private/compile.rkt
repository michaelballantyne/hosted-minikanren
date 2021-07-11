#lang racket/base

(require (for-template racket/base)
         ee-lib
         syntax/parse
         (only-in syntax/parse
                  (define/syntax-parse def/stx))
         "compile/generate-code.rkt"
         "compile/reorder-conj.rkt")

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

