#lang racket

(require "../../mk/mk.rkt")
(require "../../mk/numbers.rkt")
(require (prefix-in simple: "../../mk/simple-interp.rkt"))
(require (prefix-in full: "../../mk/full-interp.rkt"))
(require "../utils.rkt")
(require "four-fours.rkt")

(displayln "faster-minikanren")

(include "../common/bench.scm")