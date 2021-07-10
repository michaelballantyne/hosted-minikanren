#lang racket/base

(require minikanren-ee
         rackunit
         syntax/macro-testing)


(check-exn
  #rx"fresh: not a term expression"
  (lambda ()
    ; captures exceptions raised during expansion and re-raises them for runtime testing
    (convert-compile-time-error
      (run 1 (q) (== (fresh (x) (== x 1)) q)))))

