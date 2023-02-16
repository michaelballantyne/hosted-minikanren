#lang racket

(provide (all-defined-out))


(define nodes
  '(quebec
    northwest-territories
    ontario
    british-columbia
    manitoba
    alberta
    saskatchewan
    yukon
    nunavut
    newfoundland-and-labrador
    new-brunswick
    nova-scotia
    prince-edward-island))

(define edges
  '((quebec ontario)
    (quebec new-brunswick)
    (quebec newfoundland-and-labrador)
    (northwest-territories yukon)
    (northwest-territories british-columbia)
    (northwest-territories alberta)
    (northwest-territories saskatchewan)
    (northwest-territories nunavut)
    (ontario manitoba)
    (british-columbia yukon)
    (british-columbia alberta)
    (manitoba saskatchewan)
    (manitoba nunavut)
    (alberta saskatchewan)
    (new-brunswick nova-scotia)))
