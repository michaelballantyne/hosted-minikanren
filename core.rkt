#lang racket/base

; This module defines the embedded expander and compiler for the
; core language.
;
; `define-goal-macro` and define-term-macro allow sugar definitions.
;

(require "private/interface-macros.rkt"
         (for-syntax (only-in "private/compile.rkt" set-optimization-mode!)))

(provide run run* relation define-relation defrel
         quote cons
         absento symbolo stringo numbero =/= ==
         conj disj fresh
         #%rel-app #%lv-ref
         apply-relation rkt-term
         goal-from-expression expression-from-goal
         (for-syntax term-macro goal-macro)
         define-goal-macro define-term-macro
         mk-value? relation-value?
         relation-code
         relation-code/optimized
         relation-code/compiled
         (for-syntax set-optimization-mode!))

; Syntax


