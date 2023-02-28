#lang racket

(require "../../main.rkt")

(provide (all-from-out "../../main.rkt"))

(begin-for-syntax
  (set-optimization-mode! (hash
                            'constant-prop #t
                            'dead-code #t
                            'occurs-check #t
                            'unification-spec #f)))


