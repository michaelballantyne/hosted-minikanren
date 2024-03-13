#lang racket

(require (prefix-in bnd: syntax/id-set)
         racket/sequence
         racket/set
         racket/pretty
         ee-lib)

(provide
 ;; export syntax
 bound-id-set->list
 in-bound-id-set

 ;; import syntax
 immutable-bound-id-set
 bound-id-set-member?
 bound-id-set-remove
 (rename-out (bnd:bound-id-set-union bound-id-set-union)
             (bnd:bound-id-set-intersect bound-id-set-intersect)
             (bnd:bound-id-set-subtract bound-id-set-subtract)
             (bnd:bound-id-set-empty? bound-id-set-empty?))

 debug-bound-id-set-member?
 debug-bound-id-set-intersect
 )

;; (bound-id-set->list s : bound-id-set?) → list?
(define (bound-id-set->list s)
  (map flip-intro-scope (bnd:bound-id-set->list s)))

;; (in-bound-id-set s : bound-id-set?) → sequence?
(define (in-bound-id-set s)
  (sequence-map flip-intro-scope (bnd:in-bound-id-set s)))

;; (immutable-bound-id-set	[init-set : set? = null #:phase : (or/c exact-integer? #f) = (syntax-local-phase-level)]) → immutable-bound-id-set?
(define (immutable-bound-id-set [init-set '()] #:phase [phase (syntax-local-phase-level)])
  (when (bnd:immutable-bound-id-set? init-set)
    (error 'immutable-bound-id-set "You should not be trying to make an immutable bound id set out of an immutable bound id set"))
  (bnd:immutable-bound-id-set (set-map init-set flip-intro-scope) #:phase phase))

;; (bound-id-set-member? s : bound-id-set?  v : identifier?) → boolean?
(define (bound-id-set-member? s v)
  (bnd:bound-id-set-member? s (flip-intro-scope v)))

;; (bound-id-set-remove s : immutable-bound-id-set?  v : identifier?) → immutable-bound-id-set?
(define (bound-id-set-remove s v)
  (bnd:bound-id-set-remove s (flip-intro-scope v)))

;;;;;;;;;;;;

;; (debug-bound-id-set-member? s : bound-id-set?  v : identifier?) → boolean?
(define (debug-bound-id-set-member? s v)
  (displayln 'debug-bound-id-set-member?)
  (displayln 'v)
  (pretty-print (syntax-debug-info (flip-intro-scope v)))
  (displayln 's)
  (for ([id (bnd:in-bound-id-set s)])
    (pretty-print (syntax-debug-info id)))
  (displayln "")
  (bound-id-set-member? s v))


(define (debug-bound-id-set-intersect s1 s2)
  (displayln 'debug-bound-id-set-intersect)
  (displayln 's1)
  (for ([id (bnd:in-bound-id-set s1)])
    (pretty-print (syntax-debug-info id)))
  (displayln 's2)
  (for ([id (bnd:in-bound-id-set s2)])
    (pretty-print (syntax-debug-info id)))
  (displayln "")
  (bnd:bound-id-set-intersect s1 s2))
