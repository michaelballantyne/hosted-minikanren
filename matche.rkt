#lang racket
(require "main.rkt")
(require
  (for-syntax
   syntax/parse
   "private/syntax-classes.rkt"
   (only-in syntax/parse [define/syntax-parse def/stx])
   racket/syntax
   racket/list
   syntax/stx
   )
  )

(provide
 (for-space mk matche)
 defrel/matche)

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
       (def/stx [p^ (c ...) (x ...)] (compile-pattern #'p))
       (def/stx [(p^* ...) (c* ...) (x* ...)] (compile-patterns #'(p* ...)))
       #'[(p^ p^* ...) (c ... c* ...) (x ... x* ...)]]))

  ; compile pattern expression to p^, cs, and xs
  (define (compile-pattern p)
    (define (loop p)
      (syntax-parse p
        #:literals (unquote)
        ; Previous implementations use ?? for wildcard as _ isn't legal in R6.
        ; Do the same here, and raise an error on use of _ to avoid confusion.
        [(unquote . rest)
         (syntax-parse p
           #:datum-literals (?? ?)
           [(_ sym)
            #:when (or (eq? (syntax-e #'sym) '_) (eq? (syntax-e #'sym) '?))
            (raise-syntax-error #f "use ?? for wildcard" #'sym)]
           [(_ ??)
            (with-syntax ([_new ((make-syntax-introducer) #'?_)]) ; fresh but has scopes for #%lv-ref
              #'[(unquote _new) () (_new)])]
           [(_ x:id)
            #'[(unquote x) () (x)]]
           [(_ (? c:id x:id))
            #'[(unquote x) ((c x)) (x)]])]
        [(p1 . p2)
         (def/stx (p1^ (c1 ...) (x1 ...)) (loop #'p1))
         (def/stx (p2^ (c2 ...) (x2 ...)) (loop #'p2))
         #'[(p1^ . p2^) (c1 ... c2 ...) (x1 ... x2 ...)]]
        [lit #'[lit () ()]]))
    (loop p))

  ; compile matche clause to conde clause. genrates unifications to `ls`
  (define ((compile-clause ls) clause)
    (def/stx [(p ...) g ...] clause)
    (def/stx [(p^ ...) (c ...) (x ...)]
      (compile-patterns #'[p ...]))
    (def/stx (x^ ...) (remove-duplicates (syntax->list #'[x ...]) free-identifier=?))
    #`[(fresh (x^ ...) c ... (== `[p^ ...] #,ls) g ...)])

  (define-syntax-class (pattern-group vars)
    #:description "pattern group"
    (pattern (p ...)
      #:do [(define expected (length (syntax->list vars)))
            (define actual (length (syntax->list #'(p ...))))]
      #:fail-unless (= expected actual)
      (format "wrong number of patterns; expected ~a and got ~a" expected actual)))
)

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

(define-syntax defrel/matche
  (syntax-parser
    [(_ (name:id arg:id ...)
        clause ...)
     #'(defrel (name arg ...)
         (matche (arg ...)
                 clause ...))]))
