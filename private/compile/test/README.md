# The Testing Library

This testing library is designed to help pass-writers write unit tests for their pass alone
by validating the state of our IR/AST after executing a pass.

If you want to test how a pass impacts observable results of miniKanren programs, look at
integration tests or benchmarking instead.

All relevant forms are exported from `unit-test-progs.rkt`. `unit-test-infra.rkt` contains
only implementation details.

## How to set up testing

The starting template is:
```racket
(module+ test
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod ..)))
  
  ;; Write tests here
  )
```

## Testing forms

`unit-test-progs.rkt` exports four testing forms:

- `progs-equal?` checks that the input and output programs are equal modulo alpha-equivalence via `free-identifier=?`
- `progs-not-equal?` checks that `progs-equal?` is false
- `core-progs-equal?` checks that the input and out programs are equal _by expanding them to core forms_ and verifying alpha-equivalence thereof
- `core-progs-not-equal?` checks that `core-progs-equal?` is false

Additionally, it exports `generate-prog`, which is used to write actual programs to test, and comes with a DSL for making testing possible.

Each test is written in the following format:

```racket
(TESTING-FORM
  (PASS-NAME
    (generate-prog
      INPUT-PROGRAM))
  (generate-prog
    DESIRED-OUTPUT-PROGRAM))
```

For instance,

```racket
(progs-equal?
  (redundant-unify/rel
    (generate-prog
      (ir-rel ((~binder q))
        (== q 5))))
  (generate-prog
    (ir-rel ((~binder q))
      (~check (== q 5) 'skip))))
```

## The testing DSL
`generate-prog` introduces a handful of forms to make the creation of IR syntax easier. They are as follows:

1. `~binder` and `~binders` introduce `free-identifier=?` variables to the program. These must be used at any variable introduction forms.

```racket
(ir-rel ((~binder q))
  (fresh ((~binders x y))
    (== q `(,x ,y))))
```

2. `~prop` and `~props` adds arbitrary syntax properties to any enclosed piece of syntax.

```racket
(ir-rel ()
  (conj
    (~prop (== 5 5) 'foo #t)
    (== 6 (~props (cons 3 4) 'bar #t 'foo 'bar))))
```

3. `~check` and `~missing`, when they appear in the _second_ argument to a testing form, will verify that the provided syntax property either appears in the corresponding position in the first argument or doesn't appear, respectively.

```racket
(progs-equal?
  (generate-prog (ir-rel () (~prop (== 3 3) 'foo #t)))
  (generate-prog (ir-rel () (~check (== 3 3) 'foo))))

(progs-equal?
  (generate-prog (ir-rel () (== 3 3)))
  (generate-prog (ir-rel () (~missing (== 3 3) 'foo))))
```

4. `~dat-lit` indicates to the test executor that a specific syntax object should be treated as a datum literal. These should only be applied to symbols/quoted datums where the test executor may mistake it for an identifier. This can be placed on either side of the test.

```racket
(progs-equal?
  (generate-prog (ir-rel ((~binder q)) (== q (quote (~dat-lit cat)))))
  (generate-prog (ir-rel ((~binder q)) (== q (quote (~dat-lit cat))))))

(progs-equal?
  (generate-prog (ir-rel ((~binder q)) (== q 'cat)))
  (generate-prog (ir-rel ((~binder q)) (== q (quote (~dat-lit cat))))))
```

## Gotchas
- When writing syntax for relations, you *must* use `ir-rel`, and relations *do not* have names, only identifier arguments. This is a consequence of the data representation relations have in the IR.
