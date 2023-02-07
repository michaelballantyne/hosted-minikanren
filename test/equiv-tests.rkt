#lang racket
(require "../main.rkt")

(module+ test
  (require rackunit)
  (define-relation (j result1)
    (== '(1) (cons 1 result1)))

  (pretty-print (syntax->datum (relation-code j)))
  (pretty-print (syntax->datum (relation-code/compiled j)))
  (test-equal?
   "quote works just right in an equation"
   (run 1 (result1) (== '(1) (cons 1 result1)))
   '(()))

  (test-equal?
   "quasiquote works just right in an equation"
   (run 1 (result1) (== `(1) (cons 1 result1)))
   '(()))

  (test-equal?
   "quasiquote also works just fine in both places"
   (run 1 (result1) (== `(1) `(1 . ,result1)))
   '(()))


  ;; Quasiquotes a non-atom
  (define-relation (f m)
    (== `(1) m))

  ;; Quasiquotes a non-atom, must destruct to match
  (define-relation (g result1)
    (== `(1) (cons 1 result1)))

  ;; Quotes a non-atom
  (define-relation (h m)
    (== '(1) m))

  (test-equal?
   "quasiquote works well in a relation"
   (run 1 (q) (f (cons 1 q)))
   '(()))

  (test-equal?
   "quoting non-atoms works out okay in a relation"
   (run 1 (q) (h (cons 1 q)))
   '(()))

  (test-equal?
   "quasiquote unquote in a parameter to a relation call"
   (run 1 (q) (f `,q))
   '((1)))

  ;; A complicated relation to produce 2 as a value.
  (defrel (is-two? l1)
    (fresh (doozle boozle)
           (conj (== doozle (cons 7 boozle))
                 (== boozle `(2 . 5))
                 (== doozle (cons 7 (cons l1 5))))))

  (test-equal?
   "We successfully codegen the walks for variable subterms in complex t2's"
   (run 1 (l1) (is-two? l1))
   '(2))

  (test-equal?
   "We successfully unify with two racket terms on LHS and RHS"
   (let ([x 1]
         [y 1])
     (run 1 (q) (== x y)))
   '(_.0))

  )
