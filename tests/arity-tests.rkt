#lang racket

(require "../main.rkt"
         (except-in rackunit fail)
         syntax/macro-testing)

(defrel (sameo x y) (== x y))

(test-exn "Relation call with wrong arity is a static error"
          #rx"wrong number of arguments to relation; actual 3, expected 2"
          (λ ()
            (convert-compile-time-error
             (run* (q r s) (sameo q r s)))))

(test-not-exn "Relation call with correct arity is not a static error"
              (λ ()
                (convert-compile-time-error
                 (run* (q r) (sameo q r)))))

(define-syntax-rule 
  (eval-mk-module defn-or-expr ...)
  (eval #'(module eval-mk-module racket
            (require "../main.rkt")
            defn-or-expr ...)))

(test-exn "Recursive relation call with wrong arity is a static error"
          #rx"wrong number of arguments to relation; actual 3, expected 2"
          (λ ()
            (eval-mk-module 
             (defrel (bar x y)
               (conde
                 [(== x y)]
                 [(fresh (a b c)
                    (bar a b c))])))))

(test-not-exn "Recursive relation call with correct arity is not a static error"
              (λ ()
                (eval-mk-module 
                 (defrel (bar x y)
                   (conde
                     [(== x y)]
                     [(fresh (a b)
                        (bar a b))])))))

(test-exn "Mutual recursion with arity mismatch is a static error 1"
          #rx"wrong number of arguments to relation; actual 1, expected 2"
          (λ ()
            (eval-mk-module 
             (defrel (foo x y)
               (bar x))
             (defrel (bar x y)
               (foo y x)))))

(test-exn "Mutual recursion with arity mismatch is a static error 2"
          #rx"wrong number of arguments to relation; actual 1, expected 2"
          (λ ()
            (eval-mk-module 
             (defrel (foo x y)
               (bar x y))
             (defrel (bar x y)
               (foo y)))))