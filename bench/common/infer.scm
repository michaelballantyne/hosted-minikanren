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

