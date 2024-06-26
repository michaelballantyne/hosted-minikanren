There is only one version of each benchmark file now, in `bench/benchmarks`. `bench/benchmarks/bench.rkt` runs all the others. All of these files use a special magic language `benchmark-minikanren` that is swapped out by a loader.

You can use the loader directly to run an individual `.rkt` file that uses `benchmark-minikanren`. For example, to run the tests in the four-fours benchmark using hosted-minikanren as the `benchmark-minikanren`, run the following from the bench directory:

```
racket loader.rkt ../main.rkt benchmarks/four-fours.rkt test
```

or with plain faster-minikanren as the `benchmark-minikanren`:

```
racket loader.rkt base-lang.rkt benchmarks/four-fours.rkt test
```

The first argument to the `loader.rkt` program is the path to the module to load in place of `benchmark-minikanren`. The second is the module to run. The optional third argument is the submodule to run.

`bench/bench.rkt` runs the benchmark suite in hosted-minikanren and faster-minikanren. Running raco test on that file runs the test submodule of the main benchmark entry point, `bench/benchmarks/bench.rkt`. I added requires to that submodule to all the test submodules in the other benchmarks.
