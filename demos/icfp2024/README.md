# FILES

[ICFP 2024 paper miniKanren code](./paper-code.rkt). All code from the paper except external host-language libraries and the language implementation itself are here.

[A Racket-language SQLite database library](./facts.rkt). This is used for the flights database examples in the paper, but is useful generally.

[A Racket-language flights database](./flights-db.rkt). This is used for the FFI-host interop examples in the paper. The FFI examples themselves are in `paper-code.rkt`

[A trivial "demonstration-purposes" compiler](./minikanren-spec-compiler.rkt). For illustrating the language implementation code examples from the paper itself.

[Tests for the "demonstration-purposes" compiler](./test-compiler.rkt). Tests specifically for the aforementioned demonstration compiler.

[A `matche` implementation corresponding to that from the paper](../../racket-matche.rkt) The `matche` examples themselves are in `paper-code.kt`

## Compiler implementation correspondence

Code snippets not otherwise mentioned here are as described above. The demonstration compiler implements the architecture for realizing compiled, extensible multi-language DSLs in a single file. The code snippets mentioned below are in the `minikanren-spec-compiler.rkt` file. The code snippets correspond to the real implementation as follows:

## Section 2.1

### Figure 4: An EBNF-style grammar for miniKanren (left) and the corresponding `syntax-spec` version (right)

[The grammar part of the spec](../../private/spec.rkt)

## Section 3.2

### Figure 5: Host interface forms creating the boundary between Racket and miniKanren.

[The host interface part of the spec](../../private/interface-macros.rkt)

## Section 3.3

### Elaboration and extension portions of the spec

[Extension classes are listed in the spec; extension annotations are on the corresponding forms](../../private/spec.rkt)

### `list` and `fresh` implementations

[User-facing "syntax sugar" macros are with the other surface syntax forms](../../main.rkt).

### module system re-use

[The `provide` declaration exports only the required pieces](../../main.rkt)

## Section 4.2

### Figure 9: Extension to the grammar of [Figure 4](#figure-4) with additional multi-language boundary forms

[`term-from-expression` and `goal-from-expression` are also forms in the spec](../../private/spec.rkt)
[`expression-from-term` and `expression-from-goal` are also host interface forms](../../private/interface-macros.rkt)

### Figure 10: The above `goal-from-expression` sub-form from the example in [Figure 8](#figure-8) compiles to the below Racket implementation code.

runtime.rkt for unseal-and-apply
generate-code.rkt for the goal-from-expression
compile.rkt for expression-from-term and expression-from-goal.

### Figure 11: The compile-time function `compile-goal` and its helper function.

[`compile-goal` and `compile-term-variable-reference`](../../private/compile.rkt)

## Section 5

These only exist in the real compiler; the `minikanren-spec-compiler.rkt` has a trivial back-end.

### Constant folding

[All the constant folding](../../private/compile/fold.rkt)

### Dead code elimination

This actually consists of four separate passes

[Make guaranteed-failures happen as fast as possible without disturbing answer order](../../private/compile/propagate-fail.rkt)
[Replace unifications that bind names that are not used and do not escape w/`succeed`](../../private/compile/remove-no-escape.rkt)
[Remove no-ops (`succeed`s)](../../private/compile/remove-noop.rkt)
[Remove freshened, unused vars](../../private/compile/remove-unused-vars.rkt)

### Unification analysis

[Abstract interpretation discovering skippable occur-checks](../../private/compile/redundant-occurs-check.rkt)
[Abstract interpretation discovering logic variables' first use](../../private/compile/first-refs.rkt)

### Specialization

[Specialization and code generation both](../../private/compile/generate-code.rkt)

## Section 5.3

### Optimizing at the boundary with Racket

The aforementioned passes gently degrade optimizations in the presence of unknown code.
