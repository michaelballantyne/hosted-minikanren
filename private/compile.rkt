#lang racket/base

(require
 ee-lib
 syntax/stx
 syntax/parse
 syntax/id-table
 racket/function
 racket/stxparam-exptime
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

(define ((save-optimized name) g fvs fvs-free?)
  (when name
    (symbol-table-set! optimized-relation-code name g))
  g)

(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    [(run* (q ...) g)
     #`(mk:run* (q ...) #,(compile-goal #f #'g (attribute q) #t))]
    [(run n (q ...) g)
     #`(mk:run (check-natural n #'n) (q ...) #,(compile-goal #f #'g (attribute q) #t))]))

(define/hygienic (compile-relation stx name) #:expression
  (syntax-parse stx
    [(ir-rel (x ...) g)
     #`(lambda (x ...) #,(compile-goal name #'g (attribute x) #f))]))

(define (compile-expression-from-goal g)
  (define vars-in-scope (syntax-parameter-value #'surrounding-term-vars-in-scope))
  #`(goal-value #,(compile-goal #f g vars-in-scope #f)))

(define (compile-expression-from-term t)
  (define/syntax-parse current-st-var
    (syntax-parameter-value #'surrounding-current-state-var))
  ;; By runtime, we have evaluated the call to generate term, so that
  ;; produces a generated term syntax,
  ;;
  ;; Which is then evaluated to a term at rt.
  ;;
  ;; Which is then walk*ed, to a walk*ed rt value, which is at *that*
  ;; point sealed.
  ;;
  #`(seal-vars-in-term (mk:walk* #,(generate-term t) current-st-var)))

(define (compile-goal name g fvs fvs-free?)
  (define g^ (optimize-goal name g fvs fvs-free?))
  (generate-goal/entry g^ (hash-ref optimization-mode 'unification-spec)))

(define (optimize-goal name g fvs fvs-free?)

  (define (specialize-pass-to-free-variables p)
    (λ (g) (p g fvs fvs-free?)))

  (define passes
    (append
     (if (hash-ref optimization-mode 'constant-prop)
         (list fold/entry)
         '())
     (if (hash-ref optimization-mode 'dead-code)
         (list propagate-fail/entry
               remove-no-escape/entry
               remove-noop/entry
               remove-unused-vars/entry)
         '())
     (if (hash-ref optimization-mode 'occurs-check)
         (list mark-redundant-check/entry)
         '())
     (if (hash-ref optimization-mode 'unification-spec)
         (list first-refs/entry)
         '())
     (list (save-optimized name))))

  ((apply compose1r (map specialize-pass-to-free-variables passes)) g)
  )

(define (compose1r . procs) (apply compose1 (reverse procs)))

