#lang racket/base


(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require syntax-spec
         (rename-in racket/base
                    [quasiquote rkt:quasiquote]
                    [quote rkt:quote]
                    [list rkt:list])
         (prefix-in mku: "../../mk/private-unstable.rkt")
         (for-syntax racket/base
                     syntax/parse
                     (only-in syntax-spec/private/ee-lib/main lookup in-space)))

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


don't t relapps in syntax-spec
relapp needs to be last in syntpax parse dealies

don't ~literal things in the spec, but (obv) do in the parse

variable arity conj.

in the real compiler literal set can go in spec.rkt, and eliminate forms.rkt


|#

(begin-for-syntax

  (define-syntax-class goal/c
    #:description "goal expression"
    (pattern _))

  (define-syntax-class term/c
    #:description "term expression"
    (pattern _))

  (define-syntax-class bindings/c
    #:description "binding list (<id> ...)"
    (pattern (x:id ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(x ...)))
             "duplicate variable name"))

  (define-syntax-class bindings+/c
    #:description "binding list (<id> ...+)"
    (pattern (x:id ...+)
             #:fail-when (check-duplicate-identifier (syntax->list #'(x ...)))
             "duplicate variable name"))

  (define-syntax-class define-header/c
    #:description "header (<name:id> <arg:id> ...)"
    (pattern (name:id v:id ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(v ...)))
             "duplicate parameter name"))

  )

(begin-for-syntax

  (define-syntax-class boolean/c
    (pattern (~or #t #f)))

  (define-syntax-class string/c
    (pattern s:string))

  (define (maybe-interposition form-id ctx-stx)
    (let ([interposition-id ((in-space 'mk) (datum->syntax ctx-stx (syntax-e form-id)))])
      (if (lookup interposition-id (lambda (v) #t))
          interposition-id
          form-id))))

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
  s:string/c
  b:boolean/c
  ())

 (nonterminal term
  #:description "miniKanren term"
  #:bind-literal-set term-literals
  #:allow-extension term-macro

  x:term-variable
  (quote t:quoted)
  (cons t1:term t2:term)
  #;(term-from-expression e:racket-expr)
  )

 (nonterminal goal
  #:description "miniKanren goal"
  #:bind-literal-set goal-literals
  #:allow-extension goal-macro

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

 ;; TODO: probably have just one core run, do run* as a macro?
 (host-interface/expression
  (run* (x:term-variable ...) g:goal)
  #:binding (scope (bind x) g)

  (compile-run #'(run* (x ...) g)))

 (host-interface/definition
  (defrel (name:rel-name x:term-variable ...) g:goal)
  #:binding [(export name) (scope (bind x) g)]

  #:lhs
  [#'name]
  #:rhs
  [(define compiled (compile-relation #'(ir-rel (x ...) g) #'name))
   compiled])

 #;(host-interface/expression
    (expression-from-goal g:goal)
    (compile-expression-from-goal #'g))

 #;(host-interface/expression
    (expression-from-term t:term)
    (compile-expression-from-term #'t))
 )

(begin-for-syntax

  (define (compile-goal stx)
    #'3)

  (define (compile-run stx)
    #'(mku:run 1 (q) (mku:== q q)))

  (define (compile-relation stx)
    #'3)

  #;(define (compile-expression-from-goal stx)
    #'3)

  #;(define (compile-expression-from-term stx)
    #'3)

  )

(begin-for-syntax
  (define-literal-set mk-literals
    #:literal-sets (term-literals goal-literals)
    ()))

(define-syntax-rule
  (define-goal-macro m f)
  (define-extension m goal-macro f))

(define-syntax-rule
  (define-term-macro m f)
  (define-extension m term-macro f))

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
