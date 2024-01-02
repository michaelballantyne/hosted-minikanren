#lang racket/base

(provide define-facts-table assert-fact query-facts)

(require "../main.rkt" db sql
         (except-in racket/match ==)
         (for-syntax racket/base syntax/parse))

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

(define-syntax-rule (define-facts-table name [field ...])
  (define name
    (let ([conn (create-table/connect #:temporary name #:columns [field text] ...)])
      (make-facts-table conn name field ...))))

(define (assert-fact ft . args)
  (match ft
    [(facts-table c i _)
     (apply query-exec c i args)]))

(define-syntax query-facts
  (goal-macro
    (syntax-parser
      [(_ ft args ...)
       #'(goal-from-expression
           (query-facts-rt ft (list args ...)))])))

(define (query-facts-rt ft args)
  (define matching-rows (do-query (validate-table ft) (map validate-term args)))
  (unify-query-results matching-rows args))

;; Table -> Table
;; THROWS when Table is not a facts table
(define (validate-table ft)
  (cond
    [(facts-table? ft) ft]
    [else (error 'query-facts "Must query a facts table")]))

;; Term -> (Or Atom Wildcard)
;; THROWS when Term is instantiated to a non-atom
(define (validate-term t)
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

;; [Listof Atom] [Listof TermVal] -> GoalVal
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

  (define-facts-table flights [flightfrom flightto])
  (assert-fact flights "bos" "slc")
  (assert-fact flights "bos" "sea")

  (check-equal?
   (run* (q) (query-facts flights "bos" q))
   '("slc" "sea")))
