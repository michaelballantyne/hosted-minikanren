#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 racket/function
 racket/stxparam-exptime
 (for-template "runtime.rkt")
 (for-template "spec.rkt" (only-in syntax-spec with-reference-compilers))
 (for-template racket/base)
 (for-template (prefix-in mku: "../mk/private-unstable.rkt"))
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
         compile-expression-from-goal
         compile-expression-from-term
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

(define-local-symbol-table optimized-relation-code)

(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    [(run* (q ...) g)
     #`(mku:run* (q ...) #,(compile-goal #f #'g (attribute q) #t))]
    [(run n (q ...) g)
     #`(mku:run (check-natural n #'n) (q ...) #,(compile-goal #f #'g (attribute q) #t))]))

(define/hygienic (compile-relation stx name) #:expression
  (syntax-parse stx
    [(ir-rel (x ...) g)
     #`(lambda (x ...) #,(compile-goal name #'g (attribute x) #f))]))

(define (compile-expression-from-goal g)
  (define vars-in-scope
    (map syntax-local-get-shadower/including-module
         (map flip-intro-scope
              (syntax-parameter-value #'surrounding-term-vars-in-scope))))
  #`(seal-goal #,(compile-goal #f g vars-in-scope #f)))

(define (compile-expression-from-term term-exp)
  #`(expression-from-term-rt #,(generate-term term-exp)
                             #,(syntax-parameter-value #'surrounding-current-state-var)))

(define (compile-term-variable-reference compiled-term-var-id)
  #`(expression-from-term-rt #,compiled-term-var-id
                             #,(syntax-parameter-value #'surrounding-current-state-var)))

(define (compile-goal name g fvs fvs-fresh?)
  (define g^ (optimize-goal name g fvs fvs-fresh?))
  ;; It is sufficient to do the with-reference-compilers only here because an expression-from-term
  ;; outside of any goal (and thus any fresh) can have no free term variable references.
  #`(with-reference-compilers ([term-variable compile-term-variable-reference])
      #,(generate-goal/entry g^ (hash-ref optimization-mode 'unification-spec))))


(define (optimize-goal name g fvs fvs-fresh?)

  (define (specialize-pass-to-free-variables p)
    (λ (g) (p g fvs fvs-fresh?)))

  (define optimized-code-hash (list))
  (define ((save-optimized-step step) g fvs fvs-fresh?)
    (set! optimized-code-hash (append optimized-code-hash (list (list step #`(ir-rel #,fvs #,g)))))
    g)

  (define passes
    (append
     (if (hash-ref optimization-mode 'constant-prop)
         (list fold/entry
               (save-optimized-step 'constant-prop))
         '())
     (if (hash-ref optimization-mode 'dead-code)
         (list propagate-fail/entry
               remove-no-escape/entry
               remove-noop/entry
               remove-unused-vars/entry
               (save-optimized-step 'dead-code))
         '())
     (if (hash-ref optimization-mode 'occurs-check)
         (list mark-redundant-check/entry
               (save-optimized-step 'occurs-check))
         '())
     (if (hash-ref optimization-mode 'unification-spec)
         (list first-refs/entry
               (save-optimized-step 'first-refs))
         '())))

  (define g-optimized
    ((apply compose1r (map specialize-pass-to-free-variables passes)) g))

  (when name
    (symbol-table-set! optimized-relation-code name optimized-code-hash))

  g-optimized)

(define (compose1r . procs) (apply compose1 (reverse procs)))

