#lang racket/base

(require ee-lib
         syntax/parse)

(provide remove-unused-vars/rel
         remove-unused-vars/run)

(define (remove-unused-vars/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(relation (x ...) g)
     (let-values ([(g^ _) (remove-unused-vars g)])
       #`(relation (x ...) #,g^))]))

(define (remove-unused-vars/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let-values ([(g^ _) (remove-unused-vars g)])
       #`(run n (q ...) #,g^))]
    [(run* (q ...) g)
     (let-values ([(g^ _) (remove-unused-vars g)])
       #`(run* (q ...) #,g^))]))

(define (remove-unused-vars g)
  (syntax-parse g #:literal-sets (mk-literals)
    [(disj g1 g2)
     (let-values ([(g1^ g1-refs) (remove-unused-vars g1)]
                  [(g2^ g2-refs) (remove-unused-vars g2)])
       (values #`(disj #,g1^ #,g2^) (free-id-set-union g1-refs g2-refs)))]
    [(conj g1 g2)
     (let-values ([(g1^ g1-refs) (remove-unused-vars g1)]
                  [(g2^ g2-refs) (remove-unused-vars g2)])
       (values #`(conj #,g1^ #,g2^) (free-id-set-union g1-refs g2-refs)))]
    [(fresh (x ...) g)
     (let-values ([(g^ g-refs) (remove-unused-vars g)])
       (define referenced-vars (filter (λ (lv) (free-id-set-member? g-refs lv)) (syntax->list #'(x ...))))
       (define new-refs (free-id-set-subtract g-refs referenced-vars))
       (values #`(fresh (#,@referenced-vars) #,g^) new-refs))]
    [(c:unary-constraint t) (values this-syntax (term-refs #'t))]
    [(c:binary-constraint t) (values this-syntax (term-refs #'t))]
    [(#%rel-app n t ...)
     (values this-syntax
             (for/fold ([var-refs (immutable-free-id-set)])
                       ([t (syntax->list #'(t ...))])
               (free-id-set-union var-refs (term-refs t))))]
    [(apply-relation e t ...)
     (values this-syntax
             (for/fold ([var-refs (immutable-free-id-set)])
                       ([t (syntax->list #'(t ...))])
               (free-id-set-union var-refs (term-refs t))))]))

(define (term-refs t)
  (syntax-parse t #:literal-sets (mk-literals) #:literals (cons quote)
    [(#%lv-ref v)
     (immutable-free-id-set (list #'v))]
    [(rkt-term e) (immutable-free-id-set)]
    [(#%term-datum _) (immutable-free-id-set)]
    [(quote d) (immutable-free-id-set)]
    [(cons t1 t2)
     (free-id-set-union (term-refs #'t1) (term-refs #'t2))]))
