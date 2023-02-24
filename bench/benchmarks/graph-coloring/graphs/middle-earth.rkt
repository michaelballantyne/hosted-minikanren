#lang racket

(provide (all-defined-out))

(define nodes
  '(lindon
    forodwaith
    eriador
    rhovanion
    enedwaith
    rohan
    gondor
    rhun
    mordor
    khand
    harad))

(define edges
  '((forodwaith lindon)
    (forodwaith rhovanion)
    (forodwaith eriador)
    (eriador lindon)
    (rhovanion eriador)
    (enedwaith eriador)
    (enedwaith rhovanion)
    (enedwaith gondor)
    (rohan rhovanion)
    (rohan gondor)
    (rhun rohan)
    (rhun rhovanion)
    (rhun khand)
    (rhun mordor)
    (mordor gondor)
    (mordor rohan)
    (mordor harad)
    (khand mordor)
    (khand rhun)
    (khand harad)))
