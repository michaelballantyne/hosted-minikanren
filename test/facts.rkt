#lang racket/base

(provide define-facts-table assert-fact query-facts)

(require "../main.rkt" db sql
         (except-in racket/match ==)
         (for-syntax racket/base syntax/parse))

(struct facts-table [conn insert query])

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
    [(facts-table conn insert _)
     (apply query-exec (conn ft) (insert ft) args)]))

;; TODO: currently this uses a prebuilt query that doesn't leverage
;; known information about the arguments to filter at all! That should be improved.
(define (do-query ft args)
  (define c (facts-table-conn ft))
  (define query (facts-table-query ft))
  (map vector->list (query-rows c query)))

(define (unify-query-results query-res args)
  (match query-res
    ['() (expression-from-goal fail)]
    [(cons a d)
      (expression-from-goal
        (conde
          [(== (term-from-expression a) (term-from-expression args))]
          [(goal-from-expression (unify-query-results d args))]))]))

(define (query-facts-rt ft . args)
  (unify-query-results (do-query ft args) args))

(define-syntax query-facts
  (goal-macro
    (syntax-parser
      [(_ ft args ...)
       #'(goal-from-expression
           (query-facts-rt ft (expression-from-term args) ...))])))

(module+ test
  (require rackunit)
  
  (define-facts-table flights [flightfrom flightto])
  (assert-fact flights "bos" "slc")
  (assert-fact flights "bos" "sea")
  
  (check-equal?
   (run* (q) (query-facts flights "bos" q))
   '("slc" "sea")))
