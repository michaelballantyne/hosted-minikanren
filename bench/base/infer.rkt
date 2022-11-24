#lang racket
(require "../../mk/mk.rkt")
(require "../../mk/numbers.rkt")

(provide )

(include "../common/infer.scm")

;; Łukasiewicz
;; `((,p → ,q) → ,p)
;; `(((,p → ,q) → ,r) → ((,p → ,q) → (,p → ,r)))
;; ;; Meredith
;; `(,e → ((,a → ,b) → (((,d → ,a) → (,b  → ,c)) → (,a  → ,c))))
;; `(((,a → ,b) → ,c) → (,d → ((,b → (,c  → ,e)) → (,b → ,e))))


(module+ test
  (require rackunit)
  (test-equal? "1"
  (run* (q) (⊢ '() '() 17 q))
  '(ℕ))

(test-equal? "2"
  (run* (q) (⊢ '() '() '(zero? 24) q))
  '(𝔹))

(test-equal? "3"
  (run* (q) (⊢ '() '() '(zero? (sub1 24)) q))
  '(𝔹))

(test-equal? "4"
  (run* (q)
    (⊢ '() '() '(zero? (sub1 (sub1 18))) q))
  '(𝔹))

(test-equal? "5"
  (run* (q)
    (⊢ '() '() '(lambda (n) (if (zero? n) n n)) q))
  '((ℕ → ℕ)))

(test-equal? "6"
  (run* (q)
    (⊢ '() '() '((lambda (n) (zero? n)) 5) q))
  '(𝔹))

(test-equal? "7"
  (run* (q)
    (⊢ '() '() '(if (zero? 24) 3 4) q))
  '(ℕ))

(test-equal? "8"
  (run* (q)
    (⊢ '() '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
  '(𝔹))

(test-equal? "9"
  (run* (q)
    (⊢ '() '() '(lambda (x) (sub1 x)) q))
  '((ℕ → ℕ)))


(test-equal? "10"
  (run* (q)
    (⊢ '() '() '(lambda (a) (lambda (x) (+ a x))) q))
  '((ℕ → (ℕ → ℕ))))

(test-equal? "11"
  (run* (q)
    (⊢ '() '()
       '(lambda (f)
          (lambda (x)
            ((f x) x)))
         q))
  '(((_.0 → (_.0 → _.1))
     →
     (_.0 → _.1))))

(test-equal? "12"
  (run* (q)
    (⊢ '() '() '(sub1 (sub1 (sub1 6))) q))
  '(ℕ))

(test-equal? "13"
  (run 1 (q)
    (fresh (t)
      (⊢ '() '() '(lambda (f) (f f)) t)))
  '())

(test-equal? "14"
  (let ([v (run 20 (q)
             (fresh (lam a b)
               (⊢ '() '() `((,lam (,a) ,b) 5) 'ℕ)
               (== `(,lam (,a) ,b) q)))])
    ;;(pretty-print v)
    (length v))
  20)

(test-equal? "15"
  (let ([v (run 30 (q)
             (⊢ '() '() q 'ℕ))])
    ;;(pretty-print v)
    (length v))
  30)

(test-equal? "16"
  (let ([v (run 30 (q)
             (⊢ '() '() q '(ℕ → ℕ)))])
    ;;(pretty-print v)
    (length v))
  30)

(test-equal? "17"
  (let ([v (run 30 (q)
             (⊢ '() '() q '(𝔹 → ℕ)))])
    ;;(pretty-print v)
    (length v))
  30)

(test-equal? "18"
  (let ([v (run 30 (q)
             (⊢ '() '() q '(ℕ → (ℕ → ℕ))))])
    ;;(pretty-print v)
    (length v))
  30)

(test-equal? "19"
  (let ([v (run 100 (q)
             (fresh (e t)
               (⊢ '() '() e t)
               (== `(,e ,t) q)))])
    ;;(pretty-print v)
    (length v))
  100)

(test-equal? "20"
  (let ([v (run 100 (q)
             (fresh (Γx Γτ e τ)
               (⊢ Γx Γτ e τ)
               (== `(,Γx ,Γτ ,e ,τ) q)))])
    ;;(pretty-print v)
    (length v))
  100)

(test-equal? "21"
  (length
   (run 100 (q)
     (fresh (Γx Γτ v)
       (⊢ Γx Γτ `(var ,v) 'ℕ)
       (== `(,Γx ,Γτ ,v) q))))
  100)

;; As we noted in lecture, the simply-typed lambda calculus is
;; strongly-normalizing.  From this, it followed that types cannot be
;; found for fixed-point combinators such as Omega.  However, you can
;; explicitly add recursion to our language by including a special
;; operator fix in your language.  After doing so, you should be able
;; to pass the following tests below.

(define fix
  (lambda (f)
    (letrec ([g (lambda (x)
          ((f g) x))])
      g)))

(test-equal? "22"
  (run 1 (q)
    (fresh (Γx Γτ)
      (⊢ Γx Γτ
         '((fix (lambda (!)
                  (lambda (n)
                    (if (zero? n)
                        1
                        (* n (! (sub1 n)))))))
           5)
         q)))
  '(ℕ))

;; The following test demonstrates an interesting property:
;; just because a program typechecks doesn't mean it will terminate.

(test-equal? "23"
  (run 1 (q)
    (fresh (Γx Γτ)
      (⊢ Γx Γτ
         '((fix (lambda (!)
                  (lambda (n)
                    (* n (! (sub1 n))))))
           5)
         q)))
  '(ℕ))

(test-equal? "pair-1"
  (run* (q) (⊢ '() '() '(cons (zero? 1) (zero? 0)) q))
  '((𝔹 × 𝔹)))

(test-equal? "pair-2"
  (run* (q) (⊢ '() '() '(cons (zero? 1) (cons (zero? 1) (zero? 0))) q))
  '((𝔹 × (𝔹 × 𝔹))))

(test-equal? "pair-3"
  (run* (t) (⊢ '() '() '(lambda (x) (cons x x)) t))
  '((_.0 → (_.0 × _.0))))

(test-equal? "pair-4"
  (run* (t) (⊢ '() '()
               '(lambda (x)
                  (lambda (y) (cons (zero? x) (+ x y)))) t))
  '((ℕ → (ℕ → (𝔹 × ℕ)))))

(test-equal? "car-1"
;;      a function that accepts a pair of an ℕ and anything
  (run* (t) (⊢ '() '() '(lambda (x)
                          (zero? (car x))) t))
  '(((ℕ × _.0) → 𝔹)))

(test-equal? "car-2"
  (run* (t) (⊢ '() '()
               '((lambda (x)
                   (zero? (car x))) (cons 0 1)) t))
  '(𝔹))

(test-equal? "car-3"
  (run* (t) (⊢ '() '()
               '((lambda (x)
                   (zero? (car x))) (cons 0 #f)) t))
  '(𝔹))

(test-equal? "car-4"
  (run* (t) (⊢ '() '()
               '((lambda (x)
                   (zero? (car x)))
                 (cons #f 0))
               t))
  '())

(test-equal? "cdr-1"
;; a function that accepts a pair of anything and an ℕ
  (run* (t) (⊢ '() '()
               '(lambda (x)
                  (zero? (cdr x))) t))
  '(((_.0 × ℕ) → 𝔹)))

(test-equal? "cdr-2"
  (run* (t) (⊢ '() '()
               '((lambda (x)
                   (zero? (cdr x)))
                 (cons 0 1))
               t))
  '(𝔹))

(test-equal? "cdr-3"
  (run* (t) (⊢ '() '()
               '((lambda (x)
                   (zero? (cdr x))) (cons 0 #f)) t))
  '())

(test-equal? "cdr-4"
  (run* (t) (⊢ '() '()
               '((lambda (x)
                   (zero? (cdr x))) (cons #f 0)) t))
  '(𝔹))

(test-equal? "let poly 1"
 (run* (q) (⊢ '() '()
               '(let ((f (lambda (x) x)))
                  (cons (f 1) (f #t)))
               q))
 '((ℕ × 𝔹)))

(test-equal? "λ non-poly"
 (run* (q) (⊢ '() '()
              '((λ (f) (cons (f 1) (f #t))) (lambda (x) x))
               q))
 '())

(test-equal? "let poly 2"
 (run* (q) (⊢ '() '()
               '(let ((g (lambda (x) x)))
                  (let ((f (lambda (x) x)))
                    (g (cons ((g f) 1) (f (g #t))))))
               q))
 '((ℕ × 𝔹)))

(test-equal? "let checks RHS, meaning variables are bound"
 (run* (q) (⊢ '() '()
               '(let ((x x))
                  5)
               q))
 '())

(test-equal? "let checks RHS, meaning RHS expression must typecheck"
 (run* (q) (⊢ '() '()
              '(let ((x (+ #t #t)))
                  5)
               q))
 '()))
