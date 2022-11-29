#lang racket

(require "../../mk/mk.rkt")
(require "../../mk/numbers.rkt")

(provide fast-fact-7-5040 slow-fact-7-5040)

(include "../common/test-fact.scm")

(module+ test
  (require rackunit)
  (check-equal?
   (fast-fact-7-5040)
   `(, (build-num 7)))
  )
