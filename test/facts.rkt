#lang racket/base

(provide define-facts-table assert-fact query-facts)

(require "../main.rkt" db sql
         (for-syntax racket/base syntax/parse))

(struct facts-table [conn insert query])

(define (connect-and-create create-statement)
  (define conn (sqlite3-connect #:database 'memory))
  (query-exec conn create-statement)
  conn)

(define-syntax-rule
  (define-facts-table name [field ...])
  (define name
    (let ([conn (connect-and-create
                 (create-table #:temporary name #:columns [field text] ...))])
      (facts-table
       conn
       (insert #:into name #:set [field ?] ...)
       (select field ... #:from name)))))

(define (assert-fact ft . args)
  (define c (facts-table-conn ft))
  (define insert-stmt (facts-table-insert ft))
  (apply query-exec c insert-stmt args))

(define (do-query ft args)
  (define c (facts-table-conn ft))
  (define query (facts-table-query ft))
  (query-rows c query))

(define (unify-query-results query-res args)
  (if (null? query-res)
      (expression-from-goal fail)
      (expression-from-goal
       (conde
         [(== (term-from-expression (vector->list (car query-res))) (term-from-expression args))]
         [(goal-from-expression (unify-query-results (cdr query-res) args))]))))

(define (query-facts-rt ft . args)
  (define query-res (do-query ft args))
  (unify-query-results query-res args))

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
