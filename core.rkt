#lang racket/base

; This module defines the embedded expander and compiler for the
; core language.
;
; `define-goal-macro` and define-term-macro allow sugar definitions.
;

(require "private/interface-macros.rkt")

(provide run run* relation define-relation defrel
         quote cons #%term-datum #%lv-ref
         absento symbolo stringo numbero =/= ==
         conj disj fresh #%rel-app
         #%rkt-ref apply-relation rkt-term
         define-goal-macro define-term-macro
         mk-value? relation-value?
         relation-code
         relation-code/optimized
         relation-code/compiled
         (for-syntax gen:term-macro gen:goal-macro))

; Syntax


