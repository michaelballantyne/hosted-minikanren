#lang racket/base

(provide matche defrel/matche)

(require
  minikanren-ee
  
  (for-syntax racket/base
              racket/list
              syntax/stx
              syntax/parse
              minikanren-ee/private/syntax-classes))

(begin-for-syntax
  ; p is a pattern expression
  
  ; p^ is an expression to be unified with a portion of a corresponding argument
  ; c  is a predicate application to be added to the compiled goal
  ; x  is a fresh logic variable to lift

  ; apply compile-pattern to each pattern a group, and accumulate p^s, cs, and xs
  (define (compile-patterns ps)
    (syntax-parse ps
      [() #'[() () ()]]
      [(p p* ...)
       (define/syntax-parse [p^ (c ...) (x ...)] (compile-pattern #'p))
       (define/syntax-parse [(p^* ...) (c* ...) (x* ...)] (compile-patterns #'(p* ...)))
       #'[(p^ p^* ...) (c ... c* ...) (x ... x* ...)]]))

  ; compile pattern expression to p^, cs, and xs
  (define (compile-pattern p)
    (define (loop p)
      (syntax-parse p
        #:datum-literals (? cons quote list)
        [(~datum _)
         (with-syntax ([_new ((make-syntax-introducer) #'?_)]) ; fresh but has scopes for #%lv-ref
           #'[(unquote _new) () (_new)])]
        [x:id
         #'[(unquote x) () (x)]]
        [(? c:id x:id)
         #'[(unquote x) ((c x)) (x)]]
        [(cons p1 p2)
         (define/syntax-parse (p1^ (c1 ...) (x1 ...)) (loop #'p1))
         (define/syntax-parse (p2^ (c2 ...) (x2 ...)) (loop #'p2))
         #'[(p1^ . p2^) (c1 ... c2 ...) (x1 ... x2 ...)]]
        [(list p ...)
         (define/syntax-parse ((p^ (c ...) (x ...)) ...) (map loop (attribute p)))
         #'[(p^ ...) (c ... ...) (x ... ...)]]
        [(quote lit) #'[lit () ()]]))
    (loop p))

  ; compile matche clause to conde clause. genrates unifications to `ls`
  (define ((compile-clause ls) clause)
    (define/syntax-parse [(p ...) g ...] clause)
    (define/syntax-parse [(p^ ...) (c ...) (x ...)]
      (compile-patterns #'[p ...]))
    (define/syntax-parse (x^ ...) (remove-duplicates (syntax->list #'[x ...]) free-identifier=?))
    #`[(fresh (x^ ...) c ... (== `[p^ ...] #,ls) g ...)])
  
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
      "(matche (<id> ...n) [(<pat> ...n) <goal> ...] ...+)"
      (_ (arg:id ...+) [(~var pats (pattern-group #'(arg ...))) g:goal/c ...] ...+))
     #`(fresh (ls)
         (== ls `(,arg ...))
         (conde
           #,@(stx-map (compile-clause #'ls)  #'((pats g ...) ...))))]
    [(~describe
      "(matche <id> [<pat> <goal> ...] ...+)"
      (_ v:id [pat g:goal/c ...] ...+))
     #'(matche (v) [(pat) g ...] ...)]))

(define-syntax defrel/match
  (syntax-parser
    [(_ (name:id arg:id ...)
        clause ...)
     #'(defrel (name arg ...)
         (matche (arg ...)
                 clause ...))]))


(module+ test
  (require (except-in rackunit fail))

  (defrel (appendo l1 l2 l3)
    (matche (l1 l3)
      [('() _) (== l3 l2)]
      [((cons head rest) (cons head result)) (appendo rest l2 result)]))

  (check-equal?
   (run 2 (q) (appendo '(a b) '(c) q))
   '((a b c)))


)
