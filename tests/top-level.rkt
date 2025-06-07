#lang racket/load

(require "../main.rkt" rackunit)

(defrel (ones x)
  (conde
    [(== x 1)]
    [(ones x)]))

(check-equal?
  (run 2 (q) (ones q))
  '(1 1))

(defrel (bar x)
  (== 'cat x))

(defrel (call-bar)
  (bar 'cat))

(defrel (baz x)
  (fresh (e1)
    (== x `(,e1 fish))))

(defrel (call-baz)
  (fresh (x y)
    (== x (list y))
    (baz (list x y))))

(test-equal?
  "confirms that nullary relations can compile and run"
  (run 1 (q) (call-bar))
  '(_.0))

(test-equal?
  "confirms that nullary relations w/fresh can compile and run"
  (run 1 (q) (call-baz))
  '(_.0))
