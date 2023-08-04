# TODO as of 2023-08-04

## Replace the `apply-relation` ffi w/project

## Allow free lexical variables in goal expressions

## Ideally, we need to analyze what variables are actually used to do dead-code analysis even in presence of project

### Actually do those dead-code analyses, occur-check elimination, constant prop

The objective here is to show off a place where you see the mixture of a language extension w/a compiler optimization.
This is the project stuff is the compiler passes that relate to the project stuff. 

Some analyses that we wanted to/should do in compiler passes wrt "call out to racket" forms 

### auto rkt-term ifization of terms in the bodies of project

## (DONE) Add an implementation of list as a term macro 

## Reconcile Mitch’s testing framework w/syntax-spec

B/c some of the ways they handle fresh names may not be the same.

## Make the implementation of `matche` match what we have written the implementation to be

## Change the IR to do n-ary conj and disj and make suspensions explicit 

That is have a fresh/no-delay thing in there so we can tell the difference b/t a fresh from the user and a fresh inserted by our macros, so we can eliminate the latter.

So make the fresh in the IR always -no-delay and make the conde would expand to delay + n-ary disj so /that/ we can reason about the delays independently of the logical combinators and variable freshening

(b/c if you look at our benchmark results, mk/ee w/no optimizations doesn’t have the same results as faster-mk.) Search order stuff. And we have some things that looks like regressions (currently slower and changes search order.)

## (DONE) Make fresh1 have any number of variables and only 1 body.

## (OBVIATED) Multi-deeply-nested quasiquotes 

## with-reference-compilers section 

## make minikanren-ee use binding spaces so that we can use e.g. quasiquote w/ both racket and miniKanren in the same file
