#lang racket/base

; This module adds sugar atop hosted-minikanren/core and
; exports the complete language.

(require
  (rename-in
   "core.rkt"
   [conj conj2]
   [fresh fresh1]
   [run run-core]
   [run* run*-core]
   [quote quote-core])

  (rename-in racket/base
             [quasiquote rkt:quasiquote]
             [quote rkt:quote]
             [list rkt:list])

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
  conj2 fresh1 run-core run*-core quote-core)
 (for-space mk conj fresh conde quote quasiquote list)
 run run* unquote)

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
      (_ c:conde-clause/c ...+))
     #'(disj
        (conj c.g+ ...)
        ...)]))

(define-term-macro quote
  (syntax-parser
    [(~describe
      "'<datum>"
      (_ q))
     (let recur ([stx #'q])
       (syntax-parse stx #:datum-literals ()
         [(a . d) #`(cons #,(recur #'a) #,(recur #'d))]
         [(~or* v:identifier v:number v:boolean v:string) #'(quote-core v)]
         [() #'(quote-core ())]))]))

(define-term-macro quasiquote
  (syntax-parser
    [(~describe
      "`<datum>"
      (_ q))
     (let recur ([stx #'q] [level 0])
       (syntax-parse stx #:datum-literals (unquote quasiquote)
         [(unquote e)
          (if (= level 0)
              #'e
              #`(cons (quote-core unquote) #,(recur #'(e) (- level 1))))]
         [(unquote . rest)
          (raise-syntax-error 'unquote "bad unquote syntax" stx)]
         [(quasiquote e)
          #`(cons (quote-core quasiquote) #,(recur #'(e) (+ level 1)))]
         [(a . d)
          #`(cons #,(recur #'a level) #,(recur #'d level))]
         [(~or* v:identifier v:number v:boolean v:string) #'(quote-core v)]
         [() #'(quote-core ())]))]))

(define-term-macro list
  (syntax-parser
    [(~describe
      "(list <term> ...)"
      (_))
     #'(quote-core ())]
    [(~describe
      "(list <term> ...)"
      (_ t t-rest ...))
     #'(cons t (list t-rest ...))]))
