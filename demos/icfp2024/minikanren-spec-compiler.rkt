#lang racket/base

(provide (all-defined-out)
         (for-space mk (except-out (all-defined-out) core-quote fresh1))
         unquote
         (for-syntax term-macro
                     goal-macro))

(require syntax-spec
         racket/stxparam
         (prefix-in mku: "../../mk/private-unstable.rkt")
         (for-syntax racket/base
                     syntax/parse
                     (only-in syntax-spec/private/ee-lib/main
                              define/hygienic)))

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
  (core-quote t:quoted)
  (cons t1:term t2:term)
  (term-from-expression e:racket-expr)
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
  (goal-from-expression e:racket-expr)
  (fresh1 (x:term-variable ...) b:goal)
  #:binding (scope (bind x) b)
  (r:rel-name t:term ...+))

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

 ;; Added for testing.
 (host-interface/expression
   (test-goal-syntax g:goal)
   #''g)

 (host-interface/expression
   (expression-from-goal g:goal)
   (compile-expression-from-goal #'g))

 (host-interface/expression
   (expression-from-term t:term)
   (compile-expression-from-term #'t))

 )


(define-syntax-parameter surrounding-current-state-var #'mku:empty-state)

(begin-for-syntax

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

  (define (maybe-bind-surrounding-current-state-var should-bind? generated-goal)
    ;; Doing this macro-y thing so that we can convey stuff from one part of racket expansion (of the generated λ(st))
    ;; to another part of racket expansion (when we racket-expand the re-entry into miniKanren from Racket at an expression-from-term).
    ;;
    ;; What we're trying to convey is a reference to the racket variable 'st'
    (if should-bind?
        #`(λ (st)
            ((syntax-parameterize ([surrounding-current-state-var #'st])
               #,generated-goal)
             st))
        generated-goal))

  (define/hygienic (compile-goal stx) #:expression
    (syntax-parse stx
      #:datum-literals (== absento disj conj fresh1 succeed fail)
      [fail #'mku:fail]
      [succeed #'mku:succeed]
      [(== t1 t2)
       (maybe-bind-surrounding-current-state-var
        (or (contains-term-from-expression? #'t1) (contains-term-from-expression? #'t2))
        #`(mku:== #,(compile-term #'t1) #,(compile-term #'t2)))]
      [(absento t1 t2)
       (maybe-bind-surrounding-current-state-var
        (or (contains-term-from-expression? #'t1) (contains-term-from-expression? #'t2))
        #`(mku:absento #,(compile-term #'t1) #,(compile-term #'t2)))]
      [(disj g ...)
       (define/syntax-parse (g^ ...) (map compile-goal (attribute g)))
       #'(mku:conde (g^) ...)]
      [(conj g ...)
       (define/syntax-parse (g^ ...) (map compile-goal (attribute g)))
       #'(mku:conj g^ ...)]
      [(fresh1 (x ...) g) #`(mku:fresh (x ...) #,(compile-goal #'g))]
      [(goal-from-expression e) (maybe-bind-surrounding-current-state-var #t #'(unseal-goal e))]
      [(rel-name t ...)
       (define/syntax-parse (t^ ...) (map compile-term (attribute t)))
       (maybe-bind-surrounding-current-state-var
        (ormap contains-term-from-expression? (attribute t))
        #'(rel-name t^ ...))]))

  (define/hygienic (compile-term stx) #:expression
    (syntax-parse stx
      #:datum-literals (core-quote cons term-from-expression)
      [v:id #'v]
      [(core-quote d) #'(quote d)]
      [(cons t1 t2) #`(cons #,(compile-term #'t1) #,(compile-term #'t2))]
      [(term-from-expression e)
       #'e]))

  (define (compile-expression-from-goal g)
    #`(seal-goal #,(compile-goal g)))

  (define (compile-expression-from-term term-exp)
    #`(translate-term
        (mku:walk* #,(compile-term term-exp)
                   (mku:state-S #,(syntax-parameter-value #'surrounding-current-state-var)))))

  (define (contains-term-from-expression? t)
    (syntax-parse t
      #:datum-literals (core-quote cons term-from-expression)
      [v:id #f]
      [(core-quote _) #f]
      [(cons t1 t2)
       (or (contains-term-from-expression? #'t1)
           (contains-term-from-expression? #'t2))]
      [(term-from-expression _) #t]))
)

(define-extension conde goal-macro
  (syntax-parser
    [(_ (g+ ...+) ...+)
     #'(disj (conj g+ ...) ...)]))

(define-extension quote term-macro
  (syntax-parser
    [(_ q)
     (let recur ([stx #'q])
       (syntax-parse stx #:datum-literals ()
         [(a . d) #`(cons #,(recur #'a) #,(recur #'d))]
         [(~or* v:id v:number v:boolean v:string) #'(core-quote v)]
         [() #'(core-quote ())]))]))

(define-extension quasiquote term-macro
  (syntax-parser
    [(_ q)
     (let recur ([stx #'q] [level 0])
       (syntax-parse stx #:datum-literals (unquote quasiquote)
         [(unquote e)
          (if (= level 0)
              #'e
              #`(cons (core-quote unquote) #,(recur #'(e) (- level 1))))]
         [(unquote . rest)
          (raise-syntax-error 'unquote "bad unquote syntax" stx)]
         [(quasiquote e)
          #`(cons (core-quote quasiquote) #,(recur #'(e) (+ level 1)))]
         [(a . d)
          #`(cons #,(recur #'a level) #,(recur #'d level))]
         [(~or* v:id v:number v:boolean v:string) #'(core-quote v)]
         [() #'(core-quote ())]))]))

(define-extension list term-macro
  (syntax-parser
    [(_) #'(core-quote ())]
    [(_ t t-rest ...) #'(cons t (list t-rest ...))]))

(define-extension fresh goal-macro
  (syntax-parser
    [(_ (x ...) g* ...+) #'(fresh1 (x ...) (conj g* ...))]))


(struct mk-goal [proc])
(struct mk-lvar [var]
  #:methods gen:equal+hash
  [(define (equal-proc this other rec)
     (eq? (mk-lvar-var this) (mk-lvar-var other)))
   (define (hash-proc this rec)
     (rec (mk-lvar-var this)))
   (define (hash2-proc this rec)
     (rec (mk-lvar-var this)))])

(define (translate-term t)
  (cond
    [(mku:var? t) (mk-lvar t)]
    [(pair? t) (cons (translate-term (car t))
                     (translate-term (cdr t)))]
    [else t])
  )

(define (seal-goal g)
  (mk-goal g))

(define (unseal-goal goal-val)
  (if (mk-goal? goal-val)
      (mk-goal-proc goal-val)
      (raise-argument-error
       'goal-from-expression
       "mk-goal?"
       goal-val)))
