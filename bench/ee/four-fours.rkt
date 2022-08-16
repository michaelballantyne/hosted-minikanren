#lang racket

(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")

(provide four-fours)

(define-relation
 (arithmetic p r parse result)
 (fresh
  (q result1)
  (== p `(,result1 . ,q))
  (modify result1 result1 q r parse result)))

(define-relation
 (modify parse1 result1 p r parse result)
 (conde
  ((== p r) (== parse1 parse) (== result1 result))
  ((fresh
    (q parse2 result2 parse0 result0)
    (arithmetic p q parse2 result2)
    (conde
     ((== parse0 `(+ ,parse1 ,parse2)) (pluso result1 result2 result0))
     ((== parse0 `(- ,parse1 ,parse2)) (pluso result2 result0 result1))
     ((== parse0 `(* ,parse1 ,parse2)) (*o result1 result2 result0))
     ((== parse0 `(/ ,parse1 ,parse2))
      (poso result2)
      (*o result2 result0 result1))
     ((== parse0 `(sqrt ,parse))))
    (modify parse0 result0 q r parse result)))))

(define-relation (four-fours q)
  (fresh (parse)
    (arithmetic (cons (build-num 4) (cons (build-num 4) (cons (build-num 4) (cons (build-num 4) '()))))
                '()
                parse
                q)))

;; TODO build-num was originally in-line, better to use mK-ee interposition forms?
;; (define four-fours
;;   (lambda (n)
;;     (let ([p (map build-num '(4 4 4 4))]
;;           [rel-n (build-num n)])
;;       (run 1 (parse) (arithmetic p '() parse rel-n)))))

