#lang racket/base

; This module defines the embedded expander and compiler for the
; core language.
;
; `define-goal-macro` and define-term-macro allow sugar definitions.
;

(require
  ee-lib/define
  syntax/parse/define
  racket/math
  (prefix-in mk: "../mk/mk.rkt")
  "spec.rkt"
  "forms.rkt"
  "runtime.rkt"
  syntax-spec
  (for-syntax
   syntax/stx
   racket/syntax
   syntax/id-table
   (only-in ee-lib lookup)
   racket/base
   syntax/parse
   racket/generic
   "compile.rkt"
   (only-in syntax/parse [define/syntax-parse def/stx])
   "syntax-classes.rkt"))

(provide run run*  define-relation
         (rename-out [define-relation defrel])
         quote cons
         succeed fail
         absento symbolo stringo numbero =/= ==
         conj disj fresh
         #%rel-app #%lv-ref
         expression-from-goal goal-from-expression
         relation apply-relation
         term-from-expression expression-from-term
         (for-syntax term-macro goal-macro)
         define-goal-macro define-term-macro
         relation-code
         relation-code/optimized
         relation-code/compiled)

; Syntax


(begin-for-syntax
  (define-local-symbol-table expanded-relation-code)
  (define-local-symbol-table compiled-relation-code))

; run, run*, and define-relation are the interface with Racket

(syntax-spec
 (host-interface/expression
  (run n:racket-expr (x:term-variable ...) g:goal)
  #:binding {(bind x) g}

  (compile-run #'(run n (x ...) g)))

 ;; TODO: probably have just one core run, do run* as a macro?
 (host-interface/expression
  (run* (x:term-variable ...) g:goal)
  #:binding {(bind x) g}

  (compile-run #'(run* (x ...) g)))

 (host-interface/expression
  (expression-from-goal g:goal)
  (compile-expression-from-goal #'g))

 (host-interface/expression
  (expression-from-term t:term)
  (compile-expression-from-term #'t))


 (host-interface/expression
  (relation (x:term-variable ...) g:goal)
  #:binding {(bind x) g}
  ;; TODO: not 100% sure I need the ir-relation nonterminal in the spec
  #`(relation-value #,(compile-relation #'(ir-rel (x ...) g) #f)))

 (host-interface/definition
  (define-relation (name:relation-name x:term-variable ...) g:goal)
  #:binding [(export name) {(bind x) g}]

  #:lhs
  [#'name]
  #:rhs
  [(symbol-table-set! expanded-relation-code
                      #'name
                      #'(ir-rel (x ...) g))
   (define compiled (compile-relation #'(ir-rel (x ...) g) #'name))
   (symbol-table-set! compiled-relation-code
                      #'name
                      compiled)
   compiled])
 
 )

(define-syntax-rule
  (define-goal-macro m f)
  (define-syntax m (goal-macro f)))

(define-syntax-rule
  (define-term-macro m f)
  (define-syntax m (term-macro f)))

(syntax-spec
 (host-interface/expression
  (relation-code name:relation-name)
  (define code (symbol-table-ref expanded-relation-code #'name #f))
  (when (not code)
    (error 'relation-code "can only access code of relations defined in the current module"))
  #`#'#,code)

 (host-interface/expression
  (relation-code/optimized name:relation-name)
  (define code (symbol-table-ref optimized-relation-code #'name #f))
  (when (not code)
    (error 'relation-code/optimized "can only access code of relations defined in the current module"))
  #`#'#,code)
 
 (host-interface/expression
  (relation-code/compiled name:relation-name)
  (define code (symbol-table-ref compiled-relation-code #'name #f))
  (when (not code)
    (error 'relation-code/compiled "can only access code of relations defined in the current module"))
  #`#'#,code))

