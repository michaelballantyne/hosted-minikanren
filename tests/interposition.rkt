#lang racket

;; interposing works
(module a racket
  (require (rename-in "../main.rkt" [#%rel-app core-#%rel-app])
           (for-syntax racket/base syntax/parse)
           (except-in rackunit fail))

  (define-goal-macro #%rel-app
    (syntax-parser
      [(_ name arg ...)
       #'(fresh (tmpv)
           (core-#%rel-app name arg ...))]))

  (defrel (bar x)
    (== x 5))

  (defrel (foo x)
    (bar x))

  (check-equal?
   (syntax->datum
    (relation-code foo))
   '(ir-rel (x) (fresh (tmpv) (#%rel-app bar (#%lv-ref x))))))

(require 'a)

;; syntax without the interposition point in scope expands okay
(module b racket
  (require (except-in "../main.rkt" #%lv-ref)
           (except-in rackunit fail))

  (check-equal?
   (run 1 (q) (== q 5))
   '(5)))

(require 'b)
