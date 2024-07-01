#lang scribble/manual

@title{Hosted miniKanren}
@author[
 @author+email["Michael Ballantyne" "michael.ballantyne@gmail.com"]
 "Mitch Gamburg"
 "Jason Hemann"
]

@require[(for-label hosted-minikanren)]
@defmodule[hosted-minikanren]

@defform[(run num (term-variable ...) goal)
         #:grammar
         ([num @racket[natural?]])]

@defform[(run* (term-variable ...) goal)]

@defform[(defrel (rel-name term-variable ...)
           goal ...)]

@defform[(== term term)]
@defform[(=/= term term)]
@defform[(absento term term)]
@defform[(symbolo term)]
@defform[(numbero term)]
@defform[(stringo term)]

@defform[(conj term ...)]
@defform[(disj term ...)]
@defform[(fresh (term-variable ...) goal ...)]
@defform[(conde [goal ...] ...)]

