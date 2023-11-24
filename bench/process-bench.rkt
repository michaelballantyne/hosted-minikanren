#lang racket

(require text-table text-table/utils)

;; (ListOf Any), (ListOf Int), (ListOf Any), (ListOf Int) -> String
;;
;; Compute and render as string a table of speedup data, where each entry is a
;; speedup relative to the baseline-data value for the row.
(define (speedups-table benchmark-names baseline-data column-titles column-data)
  (define speedups
    (for/list ([data column-data])
      (map speedup
           baseline-data
           data)))
  (define headers (cons 'benchmark column-titles))
  (table->string (cons headers (transpose (cons benchmark-names speedups)))
                 #:->string (lambda (v) (if (number? v) (~r #:precision 2 v) (~s v)))))

;; Int, Int -> InexactReal or #f
;;
;; Compute a speedup factor for the `new` based on the baseline time of `base`.
;;
;; For example, given a new time of 100ms and a baseline time of 150ms,
;; the speedup is 1.5.
;;
;; If either time is 0, the result is #f to indicate that the speedup is undefined.
(define (speedup base new)
  (and (not (or (= 0 base) (= 0 new)))
       (exact->inexact (/ base new))))


(define (to-lines s)
  (for/list ([line (string-split s "\n")]
             #:when (not (regexp-match? #rx"Benchmark Suite" line)))
    (string-split line ": ")))

(define (to-names s) (map first (to-lines s)))
(define (to-numbers s) (map string->number (map second (to-lines s))))



(define no-opt
  #<<here
Benchmark Suite numbers:
logo-hard: 1423
Benchmark Suite all-in-fd:
all-in-fd: 752
Benchmark Suite four-fours:
256: 223
Benchmark Suite test fact:
slow fact 6 = 720: 306
Benchmark Suite oxford artifact:
love in 9900 ways: 3089
four-thrines-small: 1564
dynamic-then-lexical-3-expressions: 36
lexical-then-dynamic-3-expressions: 975
append-backward-and-small-synthesis: 579
Benchmark Suite dmitri oc bench check:
dmitri leo 801: 341
Benchmark Suite relational graph coloring:
ways to color iberia: 53
Benchmark Suite orchid graph coloring:
color kazakhstan: 351
Benchmark Suite simple interp:
complex-countdown 2: 2200
Benchmark Suite simple matche-interp:
unoptimized-matche-interp: 284
Benchmark Suite full interp:
complex-countdown 2: 2198
1 real quine: 2133
here
  )

(define all-opt
  #<<here
Benchmark Suite numbers:
logo-hard: 1062
Benchmark Suite all-in-fd:
all-in-fd: 671
Benchmark Suite four-fours:
256: 178
Benchmark Suite test fact:
slow fact 6 = 720: 210
Benchmark Suite oxford artifact:
love in 9900 ways: 2946
four-thrines-small: 1441
dynamic-then-lexical-3-expressions: 30
lexical-then-dynamic-3-expressions: 849
append-backward-and-small-synthesis: 370
Benchmark Suite dmitri oc bench check:
dmitri leo 801: 5
Benchmark Suite relational graph coloring:
ways to color iberia: 46
Benchmark Suite orchid graph coloring:
color kazakhstan: 285
Benchmark Suite simple interp:
complex-countdown 2: 378
Benchmark Suite simple matche-interp:
unoptimized-matche-interp: 274
Benchmark Suite full interp:
complex-countdown 2: 377
1 real quine: 2020
here
  )

(define prop-only
  #<<here
Benchmark Suite numbers:
logo-hard: 1085
Benchmark Suite all-in-fd:
all-in-fd: 638
Benchmark Suite four-fours:
256: 183
Benchmark Suite test fact:
slow fact 6 = 720: 271
Benchmark Suite oxford artifact:
love in 9900 ways: 3278
four-thrines-small: 1683
dynamic-then-lexical-3-expressions: 39
lexical-then-dynamic-3-expressions: 1022
append-backward-and-small-synthesis: 585
Benchmark Suite dmitri oc bench check:
dmitri leo 801: 395
Benchmark Suite relational graph coloring:
ways to color iberia: 62
Benchmark Suite orchid graph coloring:
color kazakhstan: 377
Benchmark Suite simple interp:
complex-countdown 2: 2596
Benchmark Suite simple matche-interp:
unoptimized-matche-interp: 397
Benchmark Suite full interp:
complex-countdown 2: 2741
1 real quine: 2729
here
  )

(define dead-code
  #<<here
Benchmark Suite numbers:
logo-hard: 1529
Benchmark Suite all-in-fd:
all-in-fd: 653
Benchmark Suite four-fours:
256: 237
Benchmark Suite test fact:
slow fact 6 = 720: 393
Benchmark Suite oxford artifact:
love in 9900 ways: 3677
four-thrines-small: 1857
dynamic-then-lexical-3-expressions: 53
lexical-then-dynamic-3-expressions: 1090
append-backward-and-small-synthesis: 602
Benchmark Suite dmitri oc bench check:
dmitri leo 801: 366
Benchmark Suite relational graph coloring:
ways to color iberia: 54
Benchmark Suite orchid graph coloring:
color kazakhstan: 357
Benchmark Suite simple interp:
complex-countdown 2: 2629
Benchmark Suite simple matche-interp:
unoptimized-matche-interp: 288
Benchmark Suite full interp:
complex-countdown 2: 2451
1 real quine: 2619
here
  )

(define occurs-check
  #<<here
Benchmark Suite numbers:
logo-hard: 1288
Benchmark Suite all-in-fd:
all-in-fd: 683
Benchmark Suite four-fours:
256: 218
Benchmark Suite test fact:
slow fact 6 = 720: 322
Benchmark Suite oxford artifact:
love in 9900 ways: 3496
four-thrines-small: 1726
dynamic-then-lexical-3-expressions: 42
lexical-then-dynamic-3-expressions: 1031
append-backward-and-small-synthesis: 550
Benchmark Suite dmitri oc bench check:
dmitri leo 801: 7
Benchmark Suite relational graph coloring:
ways to color iberia: 60
Benchmark Suite orchid graph coloring:
color kazakhstan: 373
Benchmark Suite simple interp:
complex-countdown 2: 430
Benchmark Suite simple matche-interp:
unoptimized-matche-interp: 292
Benchmark Suite full interp:
complex-countdown 2: 380
1 real quine: 2178
here
  )

(define faster-mk
  #<<here
Benchmark Suite numbers:
logo-hard: 1209
Benchmark Suite all-in-fd:
all-in-fd: 484
Benchmark Suite four-fours:
256: 201
Benchmark Suite test fact:
slow fact 6 = 720: 294
Benchmark Suite oxford artifact:
love in 9900 ways: 3492
four-thrines-small: 1777
dynamic-then-lexical-3-expressions: 40
lexical-then-dynamic-3-expressions: 1188
append-backward-and-small-synthesis: 756
Benchmark Suite dmitri oc bench check:
dmitri leo 801: 478
Benchmark Suite relational graph coloring:
ways to color iberia: 70
Benchmark Suite orchid graph coloring:
color kazakhstan: 446
Benchmark Suite simple interp:
complex-countdown 2: 2748
Benchmark Suite simple matche-interp:
unoptimized-matche-interp: 370
Benchmark Suite full interp:
complex-countdown 2: 2849
1 real quine: 2695
here
  )

(define column-titles (list 'faster-mk 'no-opt 'prop-only 'dead-code 'occurs-check 'all-opt))
(define column-data (map to-numbers (list faster-mk no-opt prop-only dead-code occurs-check all-opt)))

(displayln
 (speedups-table (to-names faster-mk) (car column-data) column-titles column-data))
