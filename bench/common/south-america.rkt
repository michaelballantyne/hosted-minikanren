#lang racket

(provide (all-defined-out))

(define nodes
  '(argentina
    chile
    bolivia
    paraguay
    brazil
    uruguay
    peru
    colombia
    venezuela
    guyana
    surinam
    french-guiana
    ecuador))

(define edges
  '((argentina uruguay)
    (argentina brazil)
    (argentina paraguay)
    (argentina bolivia)
    (argentina chile)
    (bolivia brazil)
    (bolivia paraguay)
    (bolivia peru)
    (brazil french-guiana)
    (brazil surinam)
    (brazil guyana)
    (brazil venezuela)
    (brazil colombia)
    (brazil peru)
    (brazil paraguay)
    (brazil uruguay)
    (chile peru)
    (colombia venezuela)
    (colombia peru)
    (colombia ecuador)
    (ecuador peru)
    (french-guiana surinam)
    (guyana surinam)
    (guyana venezuela)))
