#lang racket
(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")

(provide append-forward
         append-backward-1
         append-backward-3
         append-backward-4
         append-backward-and-small-synthesis)

(include "../common/evalo-standard.scm")

;; See "intro-examples.scm" for comparison with the `appendo` relation defined
;; directly in miniKanren.

(define (append-forward)
  (run 1 (q)
    (evalo
      `(letrec ([append
                  (lambda (l s)
                    (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))])
         (append '(a b c) '(d e)))
      q)))

(define (append-backward-1)
  (run 1 (q)  ;; NOTE: run* will never return due to infinitely many answers.
    (evalo
      `(letrec ([append
                  (lambda (l s)
                    (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))])
         (append ,q '(d e)))  ;; Notice that "q" is not quoted.
      '(a b c d e))))

(define (append-backward-3)
  (run* (q)  ;; Now run* will work, proving there is a single answer.
    (evalo
      `(letrec ([append
                  (lambda (l s)
                    (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))])
         (append ',q '(d e)))  ;; Notice that "q" is quoted.
      '(a b c d e))))

(define (append-backward-4)
  (run* (x y)
    (evalo
      `(letrec ([append
                  (lambda (l s)
                    (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))])
         (append ',x ',y))
      '(a b c d e))))

(define (append-backward-and-small-synthesis)
  (run 1 (x y)
    (evalo
      `(letrec ([append
                  (lambda (l s)
                    (if (null? l)
                      s
                      (cons ,x (append (cdr l) s))))])  ;; Note the hole: ,x
         (list (append ,y '(c d e)) (append '(f g h) '(i j))))
      '((a b c d e) (f g h i j)))))

(module+ test
  (require rackunit)

  (check-equal?
   (append-forward)
   '((a b c d e)))

  (check-equal?
   (append-backward-1)
   '('(a b c)))

  (check-equal?
   (append-backward-3)
   '((a b c)))

  (check-equal?
   (append-backward-4)
   '((() (a b c d e))
     ((a) (b c d e))
     ((a b) (c d e))
     ((a b c) (d e))
     ((a b c d) (e))
     ((a b c d e) ())))
  )
