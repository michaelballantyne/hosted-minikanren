#lang racket/base

; This module defines the embedded expander and compiler for the
; core language.
;
; `define-goal-macro` and define-term-macro allow sugar definitions.
;

(require "private/interface-macros.rkt"
         "private/runtime.rkt"
         (for-syntax (only-in "private/compile.rkt" set-optimization-mode!)))

(provide run run* relation defrel
         quote cons
         succeed fail
         absento symbolo stringo numbero =/= ==
         conj disj fresh
         #%rel-app #%lv-ref
         apply-relation
         goal-from-expression expression-from-goal
         term-from-expression expression-from-term
         (for-syntax term-macro goal-macro)
         define-goal-macro define-term-macro
         mk-value? mk-atom? mk-lvar? relation-value?
         relation-code
         relation-code/optimized
         relation-code/compiled
         (for-syntax set-optimization-mode!))

; Syntax


