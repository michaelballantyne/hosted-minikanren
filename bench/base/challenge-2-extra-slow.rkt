#lang racket
(require "../../mk/mk.rkt")

(provide twine-slow thrine-super-duper-slow)

(include "../common/evalo-standard.scm")

(define (twine-slow)
  (run 1 (p q)
       (=/= p q)
       (evalo p q)
       (evalo q p)))

(define (thrine-super-duper-slow)
  (run 1 (p q r)
       (=/= p q)
       (=/= p r)
       (=/= q r)
       (evalo p q)
       (evalo q r)
       (evalo r p)))
