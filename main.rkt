#lang racket/base

; This module adds sugar atop minikanren-ee/core and
; exports the complete language.

(require
  (rename-in
   "core.rkt"
   [conj conj2]
   [disj disj2]
   [fresh fresh1]
   [run run-core])

  (submod "core.rkt" private)
  
  (for-syntax
   syntax-generic2
   (except-in racket/base compile)
   syntax/parse
   (only-in syntax/parse [define/syntax-parse def/stx])
   "syntax-classes.rkt"
   racket/syntax
   )
  )

(provide
 (except-out
  (all-from-out "core.rkt")
  conj2 disj2 fresh1 run-core)
 conj disj fresh conde run quasiquote unquote matche)           

(define-syntax run
  (syntax-parser
    [(~describe
      "(run <number> (<id> ...+) <goal> ...+)"
      (_ n:number b:bindings+/c g+:goal/c ...+))
     #'(run-core n (b.x ...) (conj g+ ...))]))

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
       [() #'(conj g* ...)]
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

(define-term-macro unquote
  (lambda (stx)
    (raise-syntax-error 'unquote "only valid within quasiquote" stx)))

(define-term-macro quasiquote
  (syntax-parser 
    [(_ q)
     (let recur ([stx #'q])
       (syntax-parse stx #:datum-literals (unquote)
         [(unquote e) #'e]
         [(unquote . rest)
          (raise-syntax-error 'unquote "bad unquote syntax" stx)]
         [(a . d) #`(cons #,(recur #'a) #,(recur #'d))]
         [(~or* v:identifier v:number) #'(quote v)]
         [() #'(quote ())]))]))

(define-goal-macro matche
  (lambda (stx)    
    (syntax-case stx ()
      [(matche (v ...) ([pat ...] g ...) ...)
       (let ()
         (define remove-duplicates
           (lambda (ls eq-pred)
             (cond
               [(null? ls) '()]
               [(memf (lambda (x) (eq-pred (car ls) x)) (cdr ls))
                (remove-duplicates (cdr ls) eq-pred)]
               [else (cons (car ls) (remove-duplicates (cdr ls) eq-pred))])))
         (define parse-pattern
           (lambda (args pat)
             (syntax-case #`(#,args #,pat) ()
               [(() ()) #'(() () ())]
               [((a args ...) [p pat ...])
                (with-syntax ([(p^ (c ...) (x ...))
                               (parse-patterns-for-arg #'a #'p)])
                  (with-syntax ([([pat^ ...] (c^ ...) (x^ ...))
                                 (parse-pattern #'(args ...) #'[pat ...])])
                    #'([p^ pat^ ...] (c ... c^ ...) (x ... x^ ...))))]
               [x (error 'parse-pattern "bad syntax ~s ~s" args pat)])))
         (define parse-patterns-for-arg
           (lambda (v pat)
             (define loop
               (lambda (pat)
                 (syntax-case pat (unquote ?? ?) ; ?? is the new _, since _ isn't legal in R6
                   [(unquote ??)
                    (with-syntax ([_new (generate-temporary #'?_)])
                      #'((unquote _new) () (_new)))]
                   [(unquote x)
                    (when (free-identifier=? #'x v)
                      (error 'matche "argument ~s appears in pattern at an invalid depth"
                             (syntax->datum #'x)))
                    #'((unquote x) () (x))]
                   [(unquote (? c x))
                    (when (free-identifier=? #'x v)
                      (error 'matche "argument ~s appears in pattern at an invalid depth"
                             (syntax->datum #'x)))
                    #'((unquote x) ((c x)) (x))]
                   [(a . d)
                    (with-syntax ([((pat1 (c1 ...) (x1 ...))
                                    (pat2 (c2 ...) (x2 ...)))
                                   (map loop (syntax->list #'(a d)))])
                      #'((pat1 . pat2) (c1 ... c2 ...) (x1 ... x2 ...)))]
                   [x #'(x () ())])))
             (syntax-case pat (unquote ?)
               [(unquote u)
                (cond
                  [(and (identifier? #'u)
                        (free-identifier=? v #'u))
                   #'((unquote u) () ())]
                  [else (loop pat)])]
               [(unquote (? c u))
                (cond
                  [(and (identifier? #'u)
                        (free-identifier=? v #'u))
                   #'((unquote u) ((c x)) ())]
                  [else (loop pat)])]
               [else (loop pat)])))
         (unless
             (andmap (lambda (y) (= (length (syntax->datum #'(v ...))) (length y)))
                     (syntax->datum #'([pat ...] ...)))
           (error 'matche "pattern wrong length blah"))
         (with-syntax ([(([pat^ ...] (c ...) (x ...)) ...)
                        (map (lambda (y) (parse-pattern #'(v ...) y))
                             (syntax->list #'([pat ...] ...)))])
           (with-syntax ([((x^ ...) ...)
                          (map (lambda (ls)
                                 (remove-duplicates (syntax->list ls) free-identifier=?))
                               (syntax->list #'((x ...) ...)))])
             (with-syntax ([body
                            #'(conde
                               [(fresh (x^ ...) c ... (== `[pat^ ...] ls) g ...)]
                               ...)])
               #'(fresh (ls)
                   (== ls `(,v ...))
                   body)))))]
      [(matche v (pat g ...) ...)
       #'(matche (v) ([pat] g ...) ...)])))

