This implementation the existing `faster-minikanren` package (https://github.com/michaelballantyne/faster-minikanren) minikanren as its back-end, wrapping it with better static checking, program-improving transformation, and improved macro-extensibility.

`core.rkt` defines the expander, program transformation, and compilation
to `faster-minikanren`. `main.rkt` defines syntactic sugar, such as the
`defrel/match` pattern matching syntax.
