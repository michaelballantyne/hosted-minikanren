# FILES

`./paper-code.rkt`: ICFP 2024 paper miniKanren code. All code from the paper except external host-language libraries and the language implementation itself are here.

`./facts.rkt`: A Racket-language SQLite database library. This is used for the flights database examples in the paper, but is useful generally.

`./flights-db.rkt` A Racket-language flights database. This is used for the FFI-host interop examples in the paper. The FFI examples themselves are in `paper-code.kt`

`./minikanren-spec-compiler.rkt` A trivial "demonstration-purposes" compiler for illustrating the language implementation code examples from the paper itself.

`../../racket-matche.rkt` A `matche` implementation corresponding to that from the paper. The `matche` examples themselves are in `paper-code.kt`

## Compiler implementation correspondence

The correspondence between the code snippets of the demonstration purposes compiler and the real thing is as follows:

Code snippets not otherwise mentioned here are as described above.

## Section 1

### Figure 1: The architecture for realizing compiled, extensible multi-language DSLs.

## Section 2.1

### Figure 4: An EBNF-style grammar for miniKanren (left) and the corresponding `syntax-spec` version (right).

## Section 3.2

### Figure 5: Host interface forms creating the boundary between Racket and miniKanren.

## Section 3.3

### Elaboration and extension portions of the spec

### `list` and `fresh` implementations

### module system re-use

## Section 4.2

### Figure 9: Extension to the grammar of [Figure 4](#figure-4) with additional multi-language boundary forms

### Figure 10: The above `goal-from-expression` sub-form from the example in [Figure 8](#figure-8) compiles to the below Racket implementation code.

### Figure 11: The compile-time function `compile-goal` and its helper function.

## Section 5

### Constant folding

### Dead code elimination

### Unification analysis

### Specialization

## Section 5.3

### Optimizing at the boundary with Racket
