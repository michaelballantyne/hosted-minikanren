#lang racket/base

(require benchmark-minikanren
         racket/include
         (for-syntax syntax/parse
                     racket/base))

(provide (except-out (all-defined-out) appendo))

(include "../../../mk/numbers.scm")
