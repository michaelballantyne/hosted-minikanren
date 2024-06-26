#lang racket/base

(provide define-facts-table assert-fact query-facts)

(require "../../main.rkt" db sql
         syntax/macro-testing
         syntax/parse/define
         (except-in racket/match ==)
         (for-syntax racket/base syntax/parse))
;; Does this also need a private runtime kind of module?
(struct facts-table [conn insert query])
(struct wildcard ())

(define (connect-and-create create-statement)
  (define conn (sqlite3-connect #:database 'memory))
  (query-exec conn create-statement)
  conn)

(define-syntax-rule (create-table/connect . args)
  (connect-and-create (create-table . args)))

(define-syntax-rule (make-facts-table conn name field ...)
  (facts-table
   conn
   (insert #:into name #:set [field ?] ...)
   (select field ... #:from name)))


;; SIGHELP OPT [Listof [Listof String]]
;; the data Length of internal is equal to columns of table
(define-syntax-parse-rule
  (define-facts-table name:id [field:id ...]
    (~optional (~seq #:initial-data source:expr)))
  (define name
    (let* ([conn (create-table/connect #:temporary name #:columns [field text] ...)]
           [ft (make-facts-table conn name field ...)])
      (~? (for ([row (in-list source)])
            (apply assert-fact ft row)))
      ft)))

;; define/contract
(define (assert-fact ft . args)
  (match ft
    [(facts-table c i _)
     (apply query-exec c i args)]))

(define-syntax query-facts
  (goal-macro
   (syntax-parser
     [(_ ft arg ...)
      #'(goal-from-expression
         (query-facts-rt ft (list (expression-from-term arg) ...)))])))

(define (query-facts-rt ft terms)
  (define matching-rows (do-query ft (map wildcardify terms)))
  (unify-query-results matching-rows terms))

;; define/contract
;; Term -> (Or Atom Wildcard)
;; THROWS when Term is instantiated to a non-atom
(define (wildcardify t)
  (match t
    [(? mk-atom?) t]
    [(? mk-lvar?) (wildcard)]
    [_ (error 'query-facts "Term must be an atom or variable")]))

;; TODO: currently this uses a prebuilt query that doesn't leverage
;; known information about the arguments to filter at all! That should be improved.
(define (do-query ft args)
  (match ft
    [(facts-table c _ q)
     (map vector->list (query-rows c q))]))

;; [Listof [Listof Atom]] [Listof TermVal] -> GoalVal
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