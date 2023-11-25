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


;; Parsing one variant

(define (to-lines s)
  (for/list ([line (string-split s "\n")]
             #:when (not (regexp-match? #rx"Benchmark Suite" line)))
    (string-split line ": ")))

(define (to-names s) (map first (to-lines s)))
(define (to-numbers s) (map string->number (map second (to-lines s))))

;; Parsing a full run

;; String -> (ListOf (List String (ListOf String) (ListOf Int)))
(define (parse-data input)
  (define variants (group (string-split input "\n\n") 2))

  (for/list ([variant variants])
    (define name (first variant))
    (define data (second variant))
    (list name (to-names data) (to-numbers data))))

(define (group lst n)
  (if (null? lst)
      '()
  (let-values ([(first rest) (split-at lst n)])
    (cons first (group rest n)))))

;; Read a run log from stdin and write a table to stdout
(module+ main
  (define input (port->string (current-input-port)))
  (define data (parse-data input))

  (define column-titles (map first data))
  (define column-data (map third data))

  (displayln
   (speedups-table (second (first data)) (first column-data) column-titles column-data)))
