#lang racket/base

(provide (for-space mk matche) defrel/match (rename-out [defrel/match defrel/matche]))

(require
  "main.rkt"
  (for-syntax racket/base
              racket/list
              syntax/stx
              syntax/parse
              "private/syntax-classes.rkt"))

(begin-for-syntax
  ;; p is a pattern expression

  ;; p^ is a term expression to be unified with a portion of a corresponding argument
  ;; x  is a fresh logic variable to lift

  ;; apply compile-pattern to each pattern of a group, and accumulate p^s into a single term expression, and xs
  ;; (Listof Pattern) -> (Pair Term (Listof TermVar))
  (define (compile-pats ps)
    (syntax-parse ps
      [(p ...)
       (define/syntax-parse ((p^ (x ...)) ...) (map compile-pattern (attribute p)))
       (define/syntax-parse (x^ ...) (remove-duplicates (syntax->list #'(x ... ...)) bound-identifier=?))
       #'[(list p^ ...) (x^ ...)]]))

  ;; compile pattern expression to p^, and xs
  (define (compile-pattern p)
    (syntax-parse p
      #:datum-literals (? cons quote list)
      [(~datum _)
       (define/syntax-parse (_new) (generate-temporaries #'(_)))
       #'[_new (_new)]]
      [x:id
       #'[x (x)]]
      [(cons p1 p2)
       (define/syntax-parse (p1^ (x1 ...)) (compile-pattern #'p1))
       (define/syntax-parse (p2^ (x2 ...)) (compile-pattern #'p2))
       #'[(cons p1^ p2^) (x1 ... x2 ...)]]
      [(list p ...)
       (define/syntax-parse ((p^ (x ...)) ...) (map compile-pattern (attribute p)))
       #'[(p^ ...) (x ... ...)]]
      [(quote lit) #'[(quote lit) ()]]))

  (define-syntax-class (pattern-group vars)
    #:description "pattern group"
    (pattern (p ...)
      #:do [(define expected (length (syntax->list vars)))
            (define actual (length (syntax->list #'(p ...))))]
      #:fail-unless (= expected actual)
      (format "wrong number of patterns; expected ~a and got ~a" expected actual))))

(define-goal-macro matche
  (syntax-parser
    [(~describe
      "(matche (<term> ...n) [(<pat> ...n) <goal> ...] ...+)"
      (_ (arg:term/c ...+) [(~var pats (pattern-group #'(arg ...))) g:goal/c ...] ...+))
     #:with ([pats^ xs] ...) (map compile-pats (attribute pats))
     #'(fresh (ls)
         (== ls (list arg ...))
         (conde [(fresh xs (== pats^ ls) g ...)] ...))]))

(define-syntax defrel/match
  (syntax-parser
    [(_ (name:id arg:id ...)
        clause ...)
     #'(defrel (name arg ...)
         (matche (arg ...)
                 clause ...))]))


(module+ test
  (require (except-in rackunit fail)
           syntax/macro-testing)

  (defrel (appendo l1 l2 l3)
    (matche (l1 l3)
      [('() _) (== l3 l2)]
      [((cons head rest) (cons head result)) (appendo rest l2 result)]))

  (check-equal?
   (run 2 (q) (appendo '(a b) '(c) q))
   '((a b c)))

  (check-exn
   #rx"matche: wrong number of patterns; expected 2 and got 1"
   (λ ()
     (convert-compile-time-error
      (run 2 (q) (matche (q q)
                   [(a) (== q 1)])))))


  (check-exn
   #rx"matche: expected .matche"
   (λ ()
     (convert-compile-time-error
      (run 2 (q) (matche q
                   [(a) (== q 1)])))))

)
