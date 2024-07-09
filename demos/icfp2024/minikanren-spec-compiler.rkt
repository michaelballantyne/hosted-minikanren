#lang racket/base

(provide (all-defined-out)
         ;; conj
         (for-space mk (all-defined-out))
         unquote
         (for-syntax term-macro
                     goal-macro))

(require syntax-spec
         (prefix-in mku: "../../mk/mk.rkt")
         (for-syntax racket/base
                     syntax/parse
                     (only-in syntax-spec/private/ee-lib/main
                              define/hygienic)))

#|

MB NB:
order to do:
- build up like we do in the paper
1. spec w/a no-op compiler call (expand to 'of syntax)
2. write syntax-in-syntax-out tests
3. add allow extension and extension classes
3a. try writing macros

add compiler forms one-at-a-time.
(don't worry about FFI forms at first)

tests earlier on, separate host interface form - goal-stx. quoted-stx.
special no

don't t relapps in syntax-spec
relapp needs to be last in syntpax parse dealies

don't ~literal things in the spec, but (obv) do in the parse

variable arity conj.

* DONE in the real compiler literal set can go in spec.rkt, and eliminate forms.rkt

|#

(syntax-spec

 (binding-class term-variable)
 (binding-class rel-name)

 (extension-class term-macro
  #:binding-space mk)

 (extension-class goal-macro
  #:binding-space mk)

 (nonterminal quoted
  #:description "quoted value"
  s:id
  n:number
  s:string
  b:boolean
  ())

 (nonterminal term
  #:description "miniKanren term"
  #:allow-extension term-macro
  #:binding-space mk

  x:term-variable
  (quote t:quoted)
  (cons t1:term t2:term)
  #;(term-from-expression e:racket-expr)
  )

 (nonterminal goal
  #:description "miniKanren goal"
  #:allow-extension goal-macro
  #:binding-space mk

  succeed
  fail
  (== t1:term t2:term)
  (absento t1:term t2:term)
  (disj g:goal ...+)
  (conj g:goal ...+)
  (fresh1 (x:term-variable ...) b:goal)
  #:binding (scope (bind x) b)
  (r:rel-name t:term ...+)
  #;(goal-from-expression e:racket-expr))

 (host-interface/expression
  (run n:racket-expr (x:term-variable ...) g:goal)
  #:binding (scope (bind x) g)

  (compile-run #'(run n (x ...) g)))


 (host-interface/expression
  (run* (x:term-variable ...) g:goal)
  #:binding (scope (bind x) g)

  (compile-run #'(run* (x ...) g)))

 (host-interface/definition (defrel (r:rel-name x:term-variable ...) g:goal)
  #:binding [(export r) (scope (bind x) g)]
  #:lhs [#'r]
  #:rhs [(compile-relation #'(x ...) #'g)])

 (host-interface/expression
   (test-goal-syntax g:goal)
   #''g)

 #;(host-interface/expression
    (expression-from-goal g:goal)
    (compile-expression-from-goal #'g))



 #;(host-interface/expression
    (expression-from-term t:term)
    (compile-expression-from-term #'t))
 )

(begin-for-syntax

  #;(define (compile-expression-from-goal stx)
    #'3)

  #;(define (compile-expression-from-term stx)
    #'3)

(define/hygienic (compile-run stx) #:expression
  (syntax-parse stx
    #:literals (run run*)
    [(run* (q ...) g)
     #`(mku:run* (q ...) #,(compile-goal #'g))]
    [(run n (q ...) g)
     #`(mku:run n (q ...) #,(compile-goal #'g))]))

(define/hygienic (compile-relation args body) #:expression
  (syntax-parse (list args body)
    [((x ...) g)
     #`(lambda (x ...) #,(compile-goal #'g))]))

(define/hygienic (compile-goal stx) #:expression
  (syntax-parse stx
    #:datum-literals (== absento disj conj fresh1 succeed fail)
    [fail #'mku:fail]
    [succeed #'mku:succeed]
    [(== t1 t2) #`(mku:== #,(compile-term #'t1) #,(compile-term #'t2))]
    [(absento t1 t2) #`(mku:absento #,(compile-term #'t1) #,(compile-term #'t2))]
    [(disj g ...) #'mku:succeed]
    [(conj g ...) #'mku:succeed]
    [(fresh1 (x ...) g) #'mku:succeed]
    #;[(goal-from-expression e) ]
    [(rel-name t ...) #'mku:succeed]))

(define/hygienic (compile-term stx) #:expression
  (syntax-parse stx
    #:datum-literals (quote cons)
    [v:id #'v]
    [(quote d) #'(quote d)]
    [(cons t1 t2) #`(cons #,(compile-term #'t1) #,(compile-term #'t2))]
    #;[(term-from-expression e)
       #'(check-and-unseal-vars-in-term e #'e)]))

)

(define-extension conde goal-macro
  (syntax-parser
    [(_ (g+ ...+) ...+)
     #'(disj (conj g+ ...) ...)]))

;; quote-core, quote, the-quote
(define-extension the-quote term-macro
  (syntax-parser
    [(_ q)
     (let recur ([stx #'q])
       (syntax-parse stx #:datum-literals ()
         [(a . d) #`(cons #,(recur #'a) #,(recur #'d))]
         [(~or* v:id v:number v:boolean v:string) #'(quote v)]
         [() #'(quote ())]))]))

(define-extension quasiquote term-macro
  (syntax-parser
    [(_ q)
     (let recur ([stx #'q] [level 0])
       (syntax-parse stx #:datum-literals (unquote quasiquote)
         [(unquote e)
          (if (= level 0)
              #'e
              #`(cons (quote unquote) #,(recur #'(e) (- level 1))))]
         [(unquote . rest)
          (raise-syntax-error 'unquote "bad unquote syntax" stx)]
         [(quasiquote e)
          #`(cons (quote quasiquote) #,(recur #'(e) (+ level 1)))]
         [(a . d)
          #`(cons #,(recur #'a level) #,(recur #'d level))]
         [(~or* v:id v:number v:boolean v:string) #'(quote v)]
         [() #'(quote ())]))]))

(define-extension list term-macro
  (syntax-parser
    [(_) #'(quote ())]
    [(_ t t-rest ...) #'(cons t (list t-rest ...))]))

(define-extension fresh goal-macro
  (syntax-parser
    [(_ (x ...) g* ...+) #'(fresh1 (x ...) (conj g* ...))]))
