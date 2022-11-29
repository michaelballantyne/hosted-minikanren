#lang racket

(require "../../mk/mk.rkt")
(require "../../mk/numbers.rkt")
(require (prefix-in simple: "../../mk/simple-interp.rkt"))
(require (prefix-in full: "../../mk/full-interp.rkt"))
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

(displayln "faster-minikanren")

(include "../common/bench.scm")
