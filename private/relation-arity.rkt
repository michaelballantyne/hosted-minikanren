#lang racket/base

(provide record-relation-arity!
         check-rel-app-arity)

(require (for-template syntax-spec-v2)
         (only-in syntax-spec-v2/private/ee-lib/main compiled-from))

(define-persistent-symbol-table relation-arity)

(define (record-relation-arity! name args)
  (symbol-table-set! relation-arity name (length args)))

(define (check-rel-app-arity name args)
  (define actual (length args))
  (define expected (symbol-table-ref relation-arity name))
  
  (when (not (= actual expected))
    (raise-syntax-error
     #f
     (format "wrong number of arguments to relation; actual ~a, expected ~a" actual expected)
     (compiled-from name))))