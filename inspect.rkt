#lang racket/base

(require "main.rkt"
         syntax/parse
         racket/pretty
         racket/port
         fmt)

(provide print-relation-code print-relation-code/after-dead-code)

(define (prettify stx name)
  (define (prettify stx)
    (syntax-parse stx
      #:datum-literals (ir-rel == #%lv-ref #%rel-app cons quote)
      [(ir-rel (arg ...) g)
       #`(defrel (#,name arg ...) #,(prettify #'g))]
      [(== arg ...)
       #:when (syntax-property stx 'skip-occurs-check)
       #`(==/no-check . #,(map prettify (attribute arg)))]
      [(#%lv-ref v)
       #:when (syntax-property stx 'first-ref)
       #'(first-ref v)]
      [(#%lv-ref v)
       #'v]
      [(#%rel-app r arg ...)
       #`(r . #,(map prettify (attribute arg)))]
      [(cons a '())
       #`(list #,(prettify #'a))]
      [(cons a d)
       (define a^ (prettify #'a))
       (define res (prettify #'d))
       (syntax-parse res
         #:datum-literals (list)
         [(list v ...)
          #`(list #,a^ v ...)]
         [d^
          #`(cons #,a^ d^)])]
      [(a . d) #`(#,(prettify #'a) . #,(prettify #'d))]
      [_ stx]))
  (prettify stx))

(define (mk-formatter-map s)
  (cond
    [(and (string? s) (equal? s "defrel"))
     (standard-formatter-map "define")]
    [(and (string? s) (equal? s "conj"))
     (standard-formatter-map "begin")]
    [(and (string? s) (equal? s "disj"))
     (standard-formatter-map "begin")]
    [(and (string? s) (equal? s "fresh"))
     (standard-formatter-map "lambda")]
    [else (standard-formatter-map s)]))

(define (do-print stx name)
  (displayln
   (program-format
    (parameterize ([print-reader-abbreviations #t])
      (with-output-to-string (lambda () (write (syntax->datum (prettify stx name))))))
    #:formatter-map
    mk-formatter-map)))

(define-syntax-rule
  (print-relation-code r)
  (do-print (relation-code r) #'r))

(define-syntax-rule
  (print-relation-code/after-dead-code r)
  (do-print (cadr (assoc
                   'dead-code
                   (relation-code/optimized r)))
            #'r))

