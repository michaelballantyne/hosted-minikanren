#lang racket
(require minikanren-ee)

;; A regression test. Previously we had compiler bugs in the presence of shadowing.

(defrel (uh-o ls)
  (fresh (m)
    (== ls `(,m))
    (fresh (ls)
      (== ls m)
      (uh-o m))))
