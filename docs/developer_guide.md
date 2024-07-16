# Developer Guide

This document is mainly for those wishing to hack on the implementation.

# Current limitations and suggestions for improvements

- Missing some "deep flexibility" hackability of just-plain-code
- More powerful extensions
- More powerful extension hooks-stream abstraction? extensible unify?
- Additional optimizations
- Consider: export miniKanren goals like `fail` in the mk binding space?

PR's welcome! Easiest way to join in is to just email one of the ICFP paper authors!


## `faster-minikanren` subtree

The directory `./mk/` is a `git subtree` of the `faster-minikanren` repo. To use it, first add `faster-minikanren` as a remote:

```sh
git remote add faster-minikanren git@github.com:michaelballantyne/faster-minikanren.git
```

Then, to pull in new commits from upstream, pull with:

```sh
git subtree pull --prefix mk faster-minikanren master --squash
```

## Compiler and unit test framework

Documentation for the compiler unit test framework is [located with the test suite implementation](../private/compile/test/README.md)

## Code-gen syntax properties

We list all syntax properties provided for use in code-gen in [`../private/compile/prop-vars.rkt`](../private/compile/prop-vars.rkt)

