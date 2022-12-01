#lang racket

(require text-table text-table/utils)

(define master-base
#<<here
Benchmark Suite numbers:
logo-hard: 1099
Benchmark Suite all-in-fd:
all-in-fd: 0
Benchmark Suite four-fours:
256: 187
Benchmark Suite test fact:
slow fact 7 = 5040: 7158
Benchmark Suite oxford artifact:
love in 99000 ways: 42318
four-thrines-small: 1490
twine-in-standard: 180357
dynamic-then-lexical-3-expressions: 38
lexical-then-dynamic-3-expressions: 778
append-backward-and-small-synthesis: 563
scheme-in-scheme-quine-with-quasiquote: 205247
Benchmark Suite relational graph coloring:
color middle earth: 222051
Benchmark Suite orchid graph coloring:
color ireland: 14054
Benchmark Suite simple interp:
complex-countdown 2: 2396
Benchmark Suite full interp:
complex-countdown 2: 2388
6 quines: 6101
here
)

(define master-ee
#<<here
Benchmark Suite numbers:
logo-hard: 938
Benchmark Suite all-in-fd:
all-in-fd: 0
Benchmark Suite four-fours:
256: 155
Benchmark Suite test fact:
slow fact 7 = 5040: 5621
Benchmark Suite oxford artifact:
love in 99000 ways: 40640
four-thrines-small: 1375
twine-in-standard: 178777
dynamic-then-lexical-3-expressions: 35
lexical-then-dynamic-3-expressions: 678
append-backward-and-small-synthesis: 455
scheme-in-scheme-quine-with-quasiquote: 188430
Benchmark Suite relational graph coloring:
color middle earth: 196181
Benchmark Suite orchid graph coloring:
color ireland: 13294
Benchmark Suite simple interp:
complex-countdown 2: 428
Benchmark Suite full interp:
complex-countdown 2: 426
6 quines: 5696
here
)

(define opt-base
#<<here
Benchmark Suite numbers:
logo-hard: 989
Benchmark Suite all-in-fd:
all-in-fd: 0
Benchmark Suite four-fours:
256: 171
Benchmark Suite test fact:
slow fact 7 = 5040: 6500
Benchmark Suite oxford artifact:
love in 99000 ways: 40737
four-thrines-small: 1448
twine-in-standard: 179128
dynamic-then-lexical-3-expressions: 40
lexical-then-dynamic-3-expressions: 971
append-backward-and-small-synthesis: 550
scheme-in-scheme-quine-with-quasiquote: 203552
Benchmark Suite relational graph coloring:
color middle earth: 202096
Benchmark Suite orchid graph coloring:
color ireland: 13557
Benchmark Suite simple interp:
complex-countdown 2: 2225
Benchmark Suite full interp:
complex-countdown 2: 2218
6 quines: 5709
here
)

(define opt-ee
#<<here
Benchmark Suite numbers:
logo-hard: 698
Benchmark Suite all-in-fd:
all-in-fd: 0
Benchmark Suite four-fours:
256: 123
Benchmark Suite test fact:
slow fact 7 = 5040: 4432
Benchmark Suite oxford artifact:
love in 99000 ways: 37937
four-thrines-small: 1290
twine-in-standard: 158663
dynamic-then-lexical-3-expressions: 30
lexical-then-dynamic-3-expressions: 863
append-backward-and-small-synthesis: 338
scheme-in-scheme-quine-with-quasiquote: 158146
Benchmark Suite relational graph coloring:
color middle earth: 177128
Benchmark Suite orchid graph coloring:
color ireland: 10492
Benchmark Suite simple interp:
complex-countdown 2: 357
Benchmark Suite full interp:
complex-countdown 2: 356
6 quines: 5545
here
)

(define (to-names s)
  (for/list ([line (string-split s "\n")]
             #:when (not (regexp-match? #rx"Benchmark Suite" line)))
    (first (string-split line ": "))))

(define (to-numbers s)
  (for/list ([line (string-split s "\n")]
             #:when (not (regexp-match? #rx"Benchmark Suite" line)))
    (string->number (second (string-split line ": ")))))

(define (speedup base new)
  (for/list ([b-n base] [n-n new])
    (if (or (= 0 b-n) (= 0 n-n))
        0
        (exact->inexact (/ b-n n-n)))))

(define headers (list 'benchmark 'master-base 'master-ee 'opt-base 'opt-ee))

(define speedups
  (for/list ([data (list master-base master-ee opt-base opt-ee)])
    (speedup
     (to-numbers master-base)
     (to-numbers data))))

(print-table (cons headers (transpose (cons (to-names master-base) speedups)))
             #:->string (lambda (v) (if (number? v) (~r #:precision 2 v) (~s v))))

