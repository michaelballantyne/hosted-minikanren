#lang racket

(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")
(require (prefix-in simple: "../ee-stdlib/simple-interp.rkt"))
(require (prefix-in full: "../ee-stdlib/full-interp.rkt"))
(require "../utils.rkt")
(require "four-fours.rkt")
(require "all-in-fd.rkt")
(require "test-fact.rkt")
(require "relational-graph-color.rkt")
(require "orchid-graph-coloro.rkt")
(require "./challenge-1.rkt")
(require "./challenge-2.rkt")
(require "./challenge-2-extra-slow.rkt")
(require "./challenge-3.rkt")
(require "./challenge-4.rkt")
#;(require "./challenge-7.rkt")

(displayln "minikanren-ee")

(include "../common/bench.scm")
