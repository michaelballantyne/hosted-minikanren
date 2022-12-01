#lang racket
(require "../../mk/mk.rkt")
(require "../../mk/private-unstable.rkt")
(require "../../mk/matche.rkt")

(provide unoptimized-matche-interp optimized-matche-interp)

(include "../common/simple-interp-matche-compare.scm")
