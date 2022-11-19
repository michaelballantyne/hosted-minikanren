#lang racket

(require "../../main.rkt")
(require "../ee-stdlib/numbers.rkt")
(require (prefix-in simple: "../ee-stdlib/simple-interp.rkt"))
(require (prefix-in full: "../ee-stdlib/full-interp.rkt"))
(require "../utils.rkt")
(require "four-fours.rkt")

(displayln "minikanren-ee")

(include "../common/bench.scm")
