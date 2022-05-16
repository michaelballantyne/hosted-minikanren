#lang racket

;; interposing works
(module a racket
  (require (rename-in minikanren-ee [#%rel-app core-#%rel-app])
           (for-syntax racket/base syntax/parse)
           rackunit)

  (define-goal-macro #%rel-app
    (syntax-parser
      [(_ name arg ...)
       #'(fresh (tmpv)
           (core-#%rel-app name arg ...))]))

  (define-relation (bar x)
    (== x 5))

  (define-relation (foo x)
    (bar x))

  (check-equal?
   (syntax->datum
    (relation-code foo))
   '(relation (x) (fresh (tmpv) (#%rel-app bar (#%lv-ref x))))))

(require 'a)

;; syntax without the interposition point in scope expands okay
(module b racket
  (require (except-in minikanren-ee #%lv-ref)
           rackunit)

  (check-equal?
   (run 1 (q) (== q 5))
   '(5)))

(require 'b)