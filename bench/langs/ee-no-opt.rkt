#lang racket

(require "../../main.rkt")

(provide (all-from-out "../../main.rkt"))

(begin-for-syntax
  (set-optimization-mode! (hash
                            'constant-prop #f
                            'dead-code #f
                            'occurs-check #f
                            'unification-spec #f)))

