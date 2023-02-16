#lang racket

(require benchmark-minikanren
         (for-syntax syntax/parse
                     racket/base))

(provide (except-out (all-defined-out) appendo))

(include "../../../mk/numbers.scm")
