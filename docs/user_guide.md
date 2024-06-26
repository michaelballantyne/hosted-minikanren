# User Guide

The aim of this document is to explain to a DSL programmer the particular dialect of miniKanren we use, and how to read and understand error messages. We hope to do this without repeating or recapitulating existing documentation efforts.

We adopt the surface syntax of ["The Reasoned Schemer 2nd Edition"](https://mitpress.mit.edu/9780262535519/the-reasoned-schemer/). The code for the book is [available here](https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd).

This aims to be answer-order equivalent with the [faster-minikanren](https://github.com/michaelballantyne/faster-minikanren) implementation. As such [the documentation there](./mk/README.md) is correct for writing plain miniKanren programs in this implementation of the language. This equivalence is modulo the small syntactic differences between that dialect and ours (e.g. `define` vs `defrel`).

Special restrictions on host-language interop (aka FFI) and new kinds of static error messages are described in the [ICFP paper](link_to_paper).

## Additional libraries

The miniKanren relational arithmetic suite and two different implementations of a `matche` pattern matching extension are also available. Require them to make them available.

- `(require minikanren-ee/numbers)` provides the miniKanren relational arithmetic suite described in [Kiselyov et al.](https://link.springer.com/chapter/10.1007/978-3-540-78969-7_7) and "The Reasoned Schemer".
- `(require minikanren-ee/matche)` provides a Chez-scheme style `matche` implementation like that described in [Keep et al.'s](https://digitalcommons.calpoly.edu/csse_fac/83/) 2009 Scheme Workshop paper.
- `(require minikanren-ee/racket-matche)` provides the more Racket-like `matche` implementation described in the ICFP paper.

Both of the latter two provide the name `matche` and `defrel/matche`; choose one or prefix the imports to distinguish them.

## miniKanren unit tests

Because the primitive goal `fail` is also a name exported by `rackunit`, minikanren-ee users also writing unit tests should import `rackunit` as

```racket
(require (except-in rackunit fail))
```

or prefix the imports to distinguish them.
