#lang racket/base

(require "../../main.rkt"
         (except-in rackunit fail)
         syntax/macro-testing)

(check-exn
  ;; TODO: not sure if the new syntax-spec error is as good as the old one.
  #rx"run: expected miniKanren term"
  #;#rx"fresh: not a term expression"
  (lambda ()
    ; captures exceptions raised during expansion and re-raises them for runtime testing
    (convert-compile-time-error
      (run 1 (q) (== (fresh (x) (== x 1)) q)))))

