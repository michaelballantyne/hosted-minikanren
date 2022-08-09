#lang racket

(provide benchmark-suite)

(define (mean . args)
  (/ (apply + args) (length args)))

(define (observe-benchmark f)
  (define-values (_fst fst-total _fst-real _fst-gc) (time-apply f '()))
  (define-values (_snd snd-total _snd-real _snd-gc) (time-apply f '()))
  (define-values (_thd thd-total _thd-real _thd-gc) (time-apply f '()))

  (inexact->exact (round (mean fst-total snd-total thd-total))))

(define (run-benchmark name f)
  (displayln (~a name ": " (observe-benchmark f))))

(define-syntax (benchmark-suite stx)
  (syntax-case stx ()
    [(_ name [tst-name expr] ...)
     #`(begin
         (displayln (~a "Test Suite " name ":"))
         (run-benchmark tst-name (λ () expr))
         ...)]))
