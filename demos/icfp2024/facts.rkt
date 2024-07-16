#lang racket/base

(provide define-facts-table assert-fact (for-space mk query-facts))

(require "../../main.rkt" db sql
         (only-in syntax-spec define-dsl-syntax)
         syntax/macro-testing
         syntax/parse/define
         (except-in racket/match ==)
         (for-syntax racket/base syntax/parse))


;; The SQL interaction used in this extension is equivalent to something like this:
#;(begin
   (define conn (sqlite3-connect #:database 'memory))
   (query-exec conn (create-table #:temporary flights #:columns [a text] [b text]))
   (query-exec conn (insert #:into flights #:set [a ?] [b ?]) "BOS" "SLC")
   (query-exec conn (insert #:into flights #:set [a ?] [b ?]) "BOS" "SEA")
   (select a b #:from flights #:where (= b ?)))

;; TODO: might be nice to check contracts on the public API.

(struct facts-table [conn insert-statement table-name field-names])
(struct wildcard ())

(define-syntax-parse-rule
  (define-facts-table name:id [field:id ...]
    (~optional (~seq #:initial-data source:expr)))
  (define name
    (let ([ft (connect-and-create-facts-table
               (create-table #:temporary name #:columns [field text] ...)
               (insert #:into name #:set [field ?] ...)
               'name 'field ...)])
      (~? (insert-initial-data ft source))
      ft)))

;; StatementAST StatementAST Symbol . (Listof Symbol) -> Table
(define (connect-and-create-facts-table
         create-statement insert-statement name . field-names)
  (define conn (sqlite3-connect #:database 'memory))
  (query-exec conn create-statement)
  (facts-table conn insert-statement
               (make-ident-ast name) (map make-ident-ast field-names)))

(define (insert-initial-data ft source)
  (for ([row (in-list source)])
    (apply assert-fact ft row)))

;; Table (Listof Atom) -> (void)
(define (assert-fact ft . args)
  (apply query-exec
         (facts-table-conn ft)
         (facts-table-insert-statement ft)
         args))

(define-dsl-syntax query-facts goal-macro
  (syntax-parser
    [(_ ft:id term-expr ...)
     #'(goal-from-expression
        (query-facts-rt ft (list (expression-from-term term-expr) ...)))]))

;; Table (Listof TermVal) -> GoalVal
(define (query-facts-rt ft terms)
  (define matching-rows (do-query ft (map wildcardify terms)))
  (unify-query-results matching-rows terms))

;; TermVal -> (Or Atom Wildcard)
;; THROWS when term is instantiated to a non-atom
(define (wildcardify term)
  (match term
    [(? mk-atom?) term]
    [(? mk-lvar?) (wildcard)]
    [_ (error 'query-facts "Term must be an atom or variable")]))

;; Table (Listof (Or Atom Wildcard)) -> (Listof (Listof Atom))
(define (do-query ft args)
  (define fields
    (facts-table-field-names ft))
  (define-values (filtered-fields filtered-args)
    (filter-non-wildcard fields args))
  (define query
    (build-query (facts-table-table-name ft)
                 fields
                 filtered-fields))
  (map vector->list
       (apply query-rows (facts-table-conn ft) query filtered-args)))

;; (Listof IdentAST) (Listof (Or Atom Wildcard))
;;    -> (values (Listof IdentAST) (Listof Atom))
;; Filter the two lists, keeping corresponding elements of each where the `arg`
;; is not a wildcard.
(define (filter-non-wildcard fields args)
  (for/lists (filtered-fields filtered-args)
             ([field fields]
              [arg args]
              #:when (not (wildcard? arg)))
    (values field arg)))

;; IdentAST (Listof IdentAST) (Listof IdentAST) -> StatementAST
;; Build an SQL query selecting all the fields from the given table, with `where`
;; clauses filtering on equality with the filtered-fields vs a query parameter.
(define (build-query table-name fields filtered-fields)
  (if (null? filtered-fields)
      (select (SelectItem:AST ,fields)
              #:from (Ident:AST ,table-name))
      (select (SelectItem:AST ,fields)
              #:from (Ident:AST ,table-name)
              #:where (ScalarExpr:AST ,(build-and filtered-fields)))))

;; (Listof IdentAST) -> ScalarExprAST
;; `fields` should have at least one element.
;; Build an sql `and` expression with equalities for all of the given fields;
;; these should include only fields with non-wildcard values.
(define (build-and fields)
  (match fields
    [(list f) (scalar-expr-qq (= (Ident:AST ,f) ?))]
    [(cons f rest) (scalar-expr-qq
                    (and
                     (= (Ident:AST ,f) ?)
                     (ScalarExpr:AST ,(build-and rest))))]))

;; (Listof (Listof Atom)) (Listof TermVal) -> GoalVal
(define (unify-query-results query-res args)
  (match query-res
    ['() (expression-from-goal fail)]
    [(cons fst rst)
     (expression-from-goal
      (conde
        [(== fst args)]
        [(goal-from-expression (unify-query-results rst args))]))]))

(module+ test
  (require rackunit)

  (define-facts-table flights [flightfrom flightto]
    #:initial-data
    '(("BOS" "ACK")
      ("BOS" "ALB")
      ("ACK" "ALB")))

  (check-exn
   #rx"Term must be an atom or variable"
   (lambda ()
     (run* (q) (query-facts flights (cons "BOS" '()) q))))

  (check-exn
   #rx"expected miniKanren term"
   (lambda ()
     (convert-compile-time-error
      (run* (q) (query-facts flights ((lambda (x) x) "BOS") q)))))

  (define-facts-table flights2 [flightfrom flightto])

  (assert-fact flights2 "BOS" "ACK")
  (assert-fact flights2 "BOS" "ALB")

  (check-match
   (run* (q) (query-facts flights2 "BOS" q))
   (list "ACK" "ALB")))
