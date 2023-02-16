#lang racket

(provide)

(require benchmark-minikanren)

;; BUG?

;; (run 1 (q) (fresh (e α β γ) (== q (list e α β γ)) (⊢ '() '() e `((,β → ,α) → ((,γ → ,β) → (,γ → ,α))))))
;; '((((lambda (_.0) (lambda (_.1) (lambda (_.2) _.3))) ℕ _.4 _.5) (=/= ((_.0 lambda)) ((_.1 lambda))) (num _.3) (sym _.0 _.1 _.2)))
;;
;; Is this more ground that it ought to be, b/c ought it have a most general type.


;; An improved version originally based on Spenser Bauman’s B521
;; implementation w/ Poly types
;;
;; Consider, if I like my fix implementation here or if I instead want
;; some arbitrary expression `f` to type at (((x -> x) -> x) -> x) (IIRC)

;; we split the environment to facilitate shadow checking
;;
;; See e.g. the lambda line
(defrel (⊢ Γx Γτ e τ)
  (conde
    [(numbero e) (== 'ℕ τ)]
    [(== '𝔹 τ)
     (conde
       [(== e #t)]
       [(== e #f)])]
    [(fresh (ne₁ ne₂)
       (== `(* ,ne₁ ,ne₂) e)
       (absento '* Γx)
       (== 'ℕ τ)
       (⊢ Γx Γτ ne₁ 'ℕ)
       (⊢ Γx Γτ ne₂ 'ℕ))]
    [(fresh (x b τx τb)
       (absento 'lambda Γx)
       (symbolo x)
       (== `(lambda (,x) ,b) e)
       (== `(,τx → ,τb) τ)
       (⊢ `(,x . ,Γx) `((mono . ,τx) . ,Γτ) b τb))]
    [(fresh (be)
       (== `(not ,be) e)
       (absento 'not Γx)
       (== '𝔹 τ)
       (⊢ Γx Γτ be '𝔹))]
    [(fresh (ne₁ ne₂)
       (== `(+ ,ne₁ ,ne₂) e)
       (absento '+ Γx)
       (== 'ℕ τ)
       (⊢ Γx Γτ ne₁ 'ℕ)
       (⊢ Γx Γτ ne₂ 'ℕ))]
    [(fresh (ne)
       (== `(zero? ,ne) e)
       (absento '+ Γx)
       (== '𝔹 τ)
       (⊢ Γx Γτ ne 'ℕ))]
    [(fresh (ne)
       (== `(sub1 ,ne) e)
       (absento 'sub1 Γx)
       (== 'ℕ τ)
       (⊢ Γx Γτ ne 'ℕ))]
    [(fresh (te ce ae)
       (== `(if ,te ,ce ,ae) e)
       (absento 'if Γx)
       (⊢ Γx Γτ te '𝔹)
       (⊢ Γx Γτ ce τ)
       (⊢ Γx Γτ ae τ))]
    [(fresh (l r τl τr)
       (== `(cons ,l ,r) e)
       (absento 'cons Γx)
       (== `(,τl × ,τr) τ)
       (⊢ Γx Γτ l τl)
       (⊢ Γx Γτ r τr))]
    [(fresh (pe τr)
       (== `(car ,pe) e)
       (absento 'car Γx)
       (⊢ Γx Γτ pe `(,τ × ,τr)))]
    [(fresh (pe τl)
       (== `(cdr ,pe) e)
       (absento 'cdr Γx)
       (⊢ Γx Γτ pe `(,τl × ,τ)))]
    [(fresh (e₁ e₂ τx)
       (== `(,e₁ ,e₂) e)
       (⊢ Γx Γτ e₁ `(,τx → ,τ))
       (⊢ Γx Γτ e₂ τx))]
    [(fresh (f fun x)
       (== `(fix (lambda (,f) ,fun)) e)
       (absento 'fix Γx)
       (⊢ `(,f . ,Γx) `((mono . ,τ) . ,Γτ) fun τ))]
    ;; Let bindings introduce a type which is universally quantified
    ;; over the type variables of the expression being bound to a
    ;; value.
    ;;
    ;; One way to think of this is that every occurrence of the bound
    ;; variable is distinct. All usages should be able to unify
    ;; properly with the type of the expression being bound, but the
    ;; unification should not occur between all usages of the bound
    ;; variable.
    [(fresh (x e₁ b τdummy)
       (== `(let ((,x ,e₁)) ,b) e)
       (absento 'let Γx)
       (symbolo x)
       (⊢ Γx Γτ e₁ τdummy)
       (⊢ `(,x . ,Γx) `((poly ,e₁ ,Γx ,Γτ) . ,Γτ) b τ))]
    [(symbolo e)
     (=/= e 'lambda)
     (=/= e 'let)
     (=/= e '*)
     (=/= e '+)
     (=/= e 'sub1)
     (=/= e 'zero?)
     (lookupo Γx Γτ e τ)]))

(defrel (lookupo Γx Γτ x τ)
  (fresh (Γx^ Γτ^ y m/p res)
    (== `(,y . ,Γx^) Γx)
    (== `((,m/p . ,res) . ,Γτ^) Γτ)
    (conde
      [(== x y)
       ;; Distinguishing between monomorphic and polymorphic variables
       ;; complicates variable lookup.
       ;;
       ;; Only `let`s introduce polytype variables. We represent a
       ;; polytype by the expression that generated it and its type
       ;; context. With Haskell's higher rank types, we could achieve a
       ;; similar effect for lambdas.
       ;;
       ;; Monotype variables stand in for a single distinct type. The
       ;; monotype variable m cannot be unifed with both 'ℕ and '𝔹, while
       ;; the polytype variable p can. A monotype is simply the variable
       ;; corresponding to it,
       (conde
         [(== m/p 'mono) (== res τ)]
         [(== m/p 'poly)
          (fresh (eτ Γx^^ Γτ^^)
            (== res `(,eτ ,Γx^^ ,Γτ^^))
            (⊢ Γx^^ Γτ^^ eτ τ))])]
      [(=/= x y)
       (lookupo Γx^ Γτ^ x τ)])))


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
