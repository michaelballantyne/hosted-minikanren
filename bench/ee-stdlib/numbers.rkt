#lang racket

(require "../../main.rkt"
         (for-syntax syntax/parse
                     racket/base))

(provide (except-out (all-defined-out) appendo))

(include "../../mk/numbers.scm")
