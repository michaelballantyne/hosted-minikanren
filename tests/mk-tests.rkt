#lang racket/base

(require "../main.rkt" racket/include)

(include (file "../mk/test-check.scm"))

(let ()
  (include (file "../mk/==-tests.scm")))

(let ()
  (include (file "../mk/symbolo-tests.scm")))

(let ()
  (include (file "../mk/numbero-tests.scm")))

(let ()
  (include (file "../mk/symbolo-numbero-tests.scm")))

(let ()
  (include (file "../mk/stringo-tests.scm")))

(let ()
  (include (file "../mk/disequality-tests.scm")))

(let ()
  (include (file "../mk/absento-closure-tests.scm")))

(let ()
  (include (file "../mk/absento-tests.scm")))

(let ()
  (include (file "../mk/test-infer.scm")))

(let ()
  (include (file "../mk/simple-interp.scm"))
  (include (file "../mk/test-simple-interp.scm")))

(let ()
  (include (file "../mk/test-quines.scm")))

(let ()
  (include (file "../mk/numbers.scm"))
  (include (file "../mk/test-numbers.scm")))


(let () 
  (include (file "../mk/full-interp.scm"))
  (void))