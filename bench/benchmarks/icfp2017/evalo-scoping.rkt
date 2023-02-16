#lang racket/base

(provide eval-lexo eval-dyno)

(require benchmark-minikanren)

;; Interpreter implementing lexical scope

(defrel (eval-lexo expr out)
  (eval-expr-lexo expr '() out))

(defrel (eval-expr-lexo expr env out)
  (conde

      ;; --------- CONST
      ;; ρ ⊢ n ⇒ n
      [(numbero expr) (== expr out)]

      ;;               lambda ∉ ρ
      ;; ------------------------------------ ABS
      ;; ρ ⊢ (lambda (x) e) ⇒ (closure x e ρ)
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) out)
         (not-in-envo 'lambda env))]

      ;; ρ(x) = v
      ;; --------- REF
      ;; ρ ⊢ x ⇒ v
      [(symbolo expr) (lookupo expr env out)]

      ;; ρ ⊢ e1 ⇒ (closure x ec ρc)
      ;; ρ ⊢ e2 ⇒ v2
      ;; ρc, x ↦ v2 ⊢ ec ⇒ v3
      ;; -------------------------- APP
      ;;      ρ ⊢ (e1 e2) ⇒ v3
      [(fresh (e1 e2 val x body cenv new-env)
         (== `(,e1 ,e2) expr)
         (eval-expr-lexo e1 env `(closure ,x ,body ,cenv))
         (eval-expr-lexo e2 env val)
         (ext-envo x val cenv new-env)
         (eval-expr-lexo body new-env out))]))


;; Interpreter implementing dynamic scope

(defrel (eval-dyno expr out)
  (eval-expr-dyno expr '() out))

(defrel (eval-expr-dyno expr env out)
  (conde

      ;; --------- CONST
      ;; ρ ⊢ n ⇒ n
      [(numbero expr) (== expr out)]

      ;;               lambda ∉ ρ
      ;; ------------------------------------ ABS
      ;; ρ ⊢ (lambda (x) e) ⇒ (closure x e ρ)
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) out)
         (not-in-envo 'lambda env))]

      ;; ρ(x) = v
      ;; --------- REF
      ;; ρ ⊢ x ⇒ v
      [(symbolo expr) (lookupo expr env out)]

      ;; ρ ⊢ e1 ⇒ (closure x ec ρc)
      ;; ρ ⊢ e2 ⇒ v2
      ;; ρ, x ↦ v2 ⊢ ec ⇒ v3
      ;; -------------------------- APP
      ;;      ρ ⊢ (e1 e2) ⇒ v3
      [(fresh (e1 e2 val x body cenv new-env)
         (== `(,e1 ,e2) expr)
         (eval-expr-dyno e1 env `(closure ,x ,body ,cenv))
         (eval-expr-dyno e2 env val)
         (ext-envo x val env new-env)  ;; Note the use of env instead of cenv.
         (eval-expr-dyno body new-env out))]))


;; Shared utilities

(defrel (ext-envo param arg env new-env)
  (== `((,param . ,arg) . ,env) new-env))

(defrel (lookupo x env t)
  (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t)))))

(defrel (not-in-envo x env)
  (conde
    ((== '() env))
    ((fresh (y v rest)
       (== `((,y . ,v) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))
