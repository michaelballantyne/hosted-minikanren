#lang racket/base

(require (for-template racket/base)
         ee-lib
         syntax/parse
         (only-in syntax/parse
                  (define/syntax-parse def/stx))
         "compile/generate-code.rkt"
         "compile/reorder-conj.rkt"
         "compile/remove-unused-vars.rkt")

(provide
 compiled-names
 compile-run
 compile-relation)


(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    [(~or (run _ (_ ...) _)
          (run* (_ ...) _))
     (let* ([reordered (reorder-conj/run this-syntax)]
            [removed (remove-unused-vars/run reordered)])
       (generate-run removed))]))

(define/hygienic (compile-relation stx) #:expression
  (syntax-parse stx
    [(relation (x ...) g)
     (let* ([reordered (reorder-conj/rel this-syntax)]
          [removed (remove-unused-vars/rel reordered)])
       (generate-relation removed))]))

