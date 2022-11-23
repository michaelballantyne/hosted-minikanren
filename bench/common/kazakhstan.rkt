#lang racket

(provide (all-defined-out))

(define nodes
  '(mangystau
    jetisu
    pavlodar
    karaganda
    aktobe
    east-kazakhstan
    ulytau
    kostanay
    kyzylorda
    almaty
    west-kazakhstan
    north-kazakhstan
    akmola
    turkistan
    atyrau
    jambyl
    abai))

(define edges
  '((mangystau atyrau)
    (akmola north-kazakhstan)
    (almaty karaganda)
    (aktobe ulytau)
    (jetisu karaganda)
    (almaty jetisu)
    (aktobe kostanay)
    (kostanay akmola)
    (karaganda jambyl)
    (kyzylorda turkistan)
    (karaganda akmola)
    (pavlodar karaganda)
    (kyzylorda aktobe)
    (karaganda abai)
    (abai pavlodar)
    (kostanay north-kazakhstan)
    (aktobe atyrau)
    (abai jetisu)
    (kostanay ulytau)
    (west-kazakhstan aktobe)
    (atyrau west-kazakhstan)
    (akmola pavlodar)
    (ulytau kyzylorda)
    (north-kazakhstan pavlodar)
    (karaganda ulytau)
    (abai east-kazakhstan)
    (kostanay karaganda)
    (turkistan ulytau)
    (mangystau aktobe)
    (ulytau jambyl)
    (turkistan jambyl)
    (almaty jambyl)))
