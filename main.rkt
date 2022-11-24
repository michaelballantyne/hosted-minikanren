#lang racket/base

; This module adds sugar atop minikanren-ee/core and
; exports the complete language.

(require
  (rename-in
   "core.rkt"
   [conj conj2]
   [disj disj2]
   [fresh fresh1]
   [run run-core]
   [run* run*-core]
   [quote quote-core])

  (rename-in racket/base [quasiquote rkt:quasiquote]
                         [quote rkt:quote])
  
  (for-syntax
   (except-in racket/base compile)
   syntax/parse
   (only-in syntax/parse [define/syntax-parse def/stx])
   "private/syntax-classes.rkt"
   racket/syntax
   racket/list
   syntax/stx
   )
  )

(provide
 (except-out
  (all-from-out "core.rkt")
  conj2 disj2 fresh1 run-core run*-core quote-core)
 conj disj fresh conde run run* quasiquote unquote matche defrel/match quote list)

(define-syntax run
  (syntax-parser
    [(~describe
      "(run <numeric-expr> (<id> ...+) <goal> ...+)"
      (_ n:expr b:bindings+/c g+:goal/c ...+))
     #'(run-core n (b.x ...) (conj g+ ...))]))

(define-syntax run*
  (syntax-parser
    [(~describe
      "(run* (<id> ...+) <goal> ...+)"
      (_ b:bindings+/c g+:goal/c ...+))
     #'(run*-core (b.x ...) (conj g+ ...))]))

(define-goal-macro conj
  (syntax-parser
    [(~describe
      "(conj <goal> ...+)"
      (_ g:goal/c ...+))
     (syntax-parse #'(g ...)
       [(g) #'g]
       [(g1 g2 g* ...)
        #'(conj (conj2 g1 g2) g* ...)])]))

(define-goal-macro disj
  (syntax-parser
    [(~describe
      "(disj <goal> ...+)"
      (_ g:goal/c ...+))
     (syntax-parse #'(g ...)
       [(g) #'g]
       [(g1 g2 g* ...)
        #'(disj2 g1 (disj g2 g* ...))])]))

(define-goal-macro fresh
  (syntax-parser
    [(~describe
      "(fresh (<id> ...) <goal> ...+)"
      (_ b:bindings/c g*:goal/c ...+))
     (syntax-parse #'b
       [(x ...) #'(fresh1 (x ...)
                          (conj g* ...))])]))

(begin-for-syntax
  (define-syntax-class conde-clause/c
    #:description "clause [<goal> ...+]"
    (pattern (g+:goal/c ...+))))
(define-goal-macro conde
  (syntax-parser
    [(~describe
      "(conde [<goal> ...+] ...+)"
      (_ c1:conde-clause/c c*:conde-clause/c ...))
     (syntax-parse #'(c* ...)
       [()
        #'(conj c1.g+ ...)]
       [_
        #'(disj2
           (conj c1.g+ ...)
           (conde c* ...))])]))

(begin-for-syntax
  (struct term+expression [term-transformer orig-macro]
    #:methods gen:term-macro
    [(define (term-macro-transform s stx)
       ((term+expression-term-transformer s) stx))]
    #:property prop:procedure
    (λ (s stx)
      (syntax-parse stx
        [(_ . rest)
         #:with orig-macro (term+expression-orig-macro s)
         #'(orig-macro . rest)]))))

(define-syntax quote
  (term+expression
   (syntax-parser
     [(~describe
       "'<datum>"
       (_ q))
      (let recur ([stx #'q])
        (syntax-parse stx #:datum-literals ()
          [(a . d) #`(cons #,(recur #'a) #,(recur #'d))]
          [(~or* v:identifier v:number v:boolean v:string) #'(quote-core v)]
          [() #'(quote-core ())]))])
   #'rkt:quote))

(define-syntax quasiquote
  (term+expression
   (syntax-parser 
     [(~describe
       "`<datum>"
       (_ q))
      (let recur ([stx #'q])
        (syntax-parse stx #:datum-literals (unquote)
          [(unquote e) #'e]
          [(unquote . rest)
           (raise-syntax-error 'unquote "bad unquote syntax" stx)]
          [(a . d) #`(cons #,(recur #'a) #,(recur #'d))]
          [(~or* v:identifier v:number v:boolean v:string) #'(quote-core v)]
          [() #'(quote-core ())]))])
   #'rkt:quasiquote))

(define-syntax list
  (term+expression
   (syntax-parser
     [(~describe
       "(list <term> ...)"
       (_))
      #'(quote-core ())]
     [(~describe
       "(list <term> ...)"
       (_ t t-rest ...))
      #'(cons t (list t-rest ...))])
   #'rkt:list))

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

