#lang racket

(require "../../mk/mk.rkt")
(require "../common/functional-graph-split.rkt")

(provide
 do-australia
 do-canada
 do-iberia
 do-south-america
 do-kazakhstan
 do-middle-earth
 do-ireland
 do-mexico
 do-america)

(require (prefix-in australia: "../common/australia.rkt"))
(require (prefix-in america: "../common/america.rkt"))
(require (prefix-in canada: "../common/canada.rkt"))
(require (prefix-in middle-earth: "../common/middle-earth.rkt"))
(require (prefix-in mexico: "../common/mexico.rkt"))
(require (prefix-in iberia: "../common/iberia.rkt"))
(require (prefix-in south-america: "../common/south-america.rkt"))
(require (prefix-in kazakhstan: "../common/kazakhstan.rkt"))
(require (prefix-in ireland: "../common/ireland.rkt"))

(include "../common/orchid-graph-coloro.scm")
