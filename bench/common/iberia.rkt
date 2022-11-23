#lang racket

(provide (all-defined-out))


(define nodes
  '(spain
    portugal
    cerdagne
    andorra
    gibraltar))

(define edges
  '((portugal spain)
    (andorra spain)
    (cerdagne spain)
    (gibraltar spain)))
