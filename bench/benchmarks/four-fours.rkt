#lang racket

(provide four-fours four-fours-at-12-check)

(require benchmark-minikanren)
(require "stdlib/numbers.rkt")

(defrel (arithmetic p r parse result)
 (fresh
  (q result1)
  (== p `(,result1 . ,q))
  (modify result1 result1 q r parse result)))

(defrel (modify parse1 result1 p r parse result)
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

(define four-fours
  (lambda (n)
    (let ([p (map build-num '(4 4 4 4))]
          [rel-n (build-num n)])
      (run 1 (parse) (arithmetic p '() parse rel-n)))))

(define four-fours-at-12-check
  (lambda ()
    (let ([p (map build-num '(4 4 4 4))]
          [rel-n (build-num 12)])
      (run 1 (parse) (arithmetic p '() '(* (- (0 0 1) (/ (0 0 1) (0 0 1))) (0 0 1)) rel-n)))))

(module+ test
  (require rackunit)
  (check-equal?
   (run 1 (parse) (modify '(0 0 1) '(0 0 1) '((0 0 1)) '() '(/ (0 0 1) (0 0 1)) '(1)))
   '(_.0))

  (check-equal?
   (run 1 (parse) (modify '(0 0 1) '(0 0 1) '((0 0 1) (0 0 1)) '() parse '(1 1)))
   '((- (0 0 1) (/ (0 0 1) (0 0 1)))))

  (check-equal?
   (run 1 (result0) (pluso '(1) result0 '(0 0 1)))
   '((1 1)))

  (check-equal?
   (run 1 (result1) (pluso '(1) `(1 . ,result1) '(0 1)))
   '(())))

