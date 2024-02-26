#lang racket
(require minikanren-ee)

#;(defrel (uh-o ls)
  (fresh (m)
    (== ls `(,m))
    (fresh (ls)
      (== ls m)
      (uh-o m))))

(error 'uh-o "test program disabled because of compile time infinite loop")

;; (pretty-print (syntax->datum (relation-code uh-o)))
;; (pretty-print (syntax->datum (relation-code/compiled uh-o)))
