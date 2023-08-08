#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 racket/function
 (for-template "runtime.rkt")
 (for-template racket/base)
 (for-template (prefix-in mk: "../mk/mk.rkt"))
 (only-in syntax/parse [define/syntax-parse def/stx])
 (only-in threading ~>)
 "syntax-classes.rkt"
 "compile/generate-code.rkt"
 "compile/reorder-conj.rkt"
 "compile/fold.rkt"
 "compile/first-refs.rkt"
 "compile/remove-noop.rkt"
 "compile/remove-unused-vars.rkt"
 "compile/remove-no-escape.rkt"
 "compile/propagate-fail.rkt"
 "compile/redundant-occurs-check.rkt"
 (for-template "forms.rkt"))

(provide compile-run
         compile-relation
         optimized-relation-code
         set-optimization-mode!)

(define all-opts (hash
                   'constant-prop #t
                   'dead-code #t
                   'occurs-check #t
                   'unification-spec #t))

(define optimization-mode all-opts)

(define (set-optimization-mode! mode)
  (set! optimization-mode mode))

(define (compose1r . procs) (apply compose1 (reverse procs)))

(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    [(~or (run _ (_ ...) _)
          (run* (_ ...) _))

     (define passes
       (append
         (if (hash-ref optimization-mode 'constant-prop)
           (list fold/run)
           '())
         (if (hash-ref optimization-mode 'dead-code)
           (list propagate-fail/run
                 remove-no-escape/run
                 remove-noop/run
                 remove-unused-vars/run)
           '())
         (if (hash-ref optimization-mode 'occurs-check)
           (list mark-redundant-check/run)
           '())
         (if (hash-ref optimization-mode 'unification-spec)
           (list
             first-refs/run
             generate-specialized/run)
           (list
             generate-plain/run))))

     ;(displayln `(compiling ,stx))
     ;(displayln passes)

     ((apply compose1r passes) this-syntax)]))

(define-local-symbol-table optimized-relation-code)

(define ((save-optimized name) stx)
  (when name
    (symbol-table-set! optimized-relation-code name stx))
  stx)

(define/hygienic (compile-relation stx name) #:expression
  (syntax-parse stx
    [(ir-rel (x ...) g)


     (define passes
       (append
         (if (hash-ref optimization-mode 'constant-prop)
           (list fold/rel)
           '())
         (if (hash-ref optimization-mode 'dead-code)
           (list propagate-fail/rel
                 remove-no-escape/rel
                 remove-noop/rel
                 remove-unused-vars/rel)
           '())
         (if (hash-ref optimization-mode 'occurs-check)
           (list mark-redundant-check/rel)
           '())
         (list (save-optimized name))
         (if (hash-ref optimization-mode 'unification-spec)
           (list
             first-refs/rel
             generate-specialized/rel)
           (list
             generate-plain/rel))))

     ;(displayln `(compiling ,name))
     ;(displayln passes)

     ((apply compose1r passes) this-syntax)]))
