#lang racket

(require syntax/macro-testing
         rackunit
         "unit-test-infra.rkt"
         (for-syntax syntax/parse/define syntax/parse "unit-test-infra.rkt"))

(provide generate-prog
         progs-equal?)
         ;; progs-not-equal?
         ;; core-progs-equal?)
         ;; core-progs-not-equal?)

;; FIXME incorrect source locations are printed on test failures
;; (define-syntax (progs-equal? stx)
;;   (syntax-parse stx
;;     [(_ p1 p2)
;;      #`(let ([res-info #,(generate-test-run #'p1 #'p2)])
;;          (with-check-info*
;;            (list (make-check-location #,(explode-srcloc (syntax-srcloc stx))))
;;            (λ () (check-true (car res-info) (cdr res-info)))))]))


(begin-for-syntax
  (define (run-test p1 p2 cmp)
    (get-test-result (cmp p1 p2)))

  (define (run-ir-test p1 p2) (run-test p1 p2 alpha=?))
  (define (run-core-test p1 p2 [new-ids '()])
    (run-test p1 p2 (λ (p1 p2) (core/alpha=? p1 p2 new-ids))))

  (define (gen-location-info stx)
    (let ([src  (syntax-source stx)]
          [line (syntax-line stx)]
          [col  (syntax-column stx)]
          [pos  (syntax-position stx)]
          [span (syntax-span stx)])
      #`(make-check-location (list #,src #,line #,col #,pos #,span)))))

(define-syntax (progs-equal? stx)
  (syntax-parse stx
    [(_ p1 p2)
     #`(let ([res-info (phase1-eval (run-ir-test p1 p2))])
         (with-check-info*
           (list #,(gen-location-info stx))
           (λ () (check-true (car res-info) (cdr res-info)))))]))

(define-syntax (progs-not-equal? stx)
  (syntax-parse stx
    [(_ p1 p2)
     #`(let ([res-info (phase1-eval (run-ir-test p1 p2))])
         (with-check-info*
           (list #,(gen-location-info stx))
           (λ () (check-false (car res-info) (cdr res-info)))))]))

(define-syntax (core-progs-equal? stx)
  (syntax-parse stx
    [(_ p1 p2 (~optional (~seq #:new-ids new-ids)))
     (with-syntax ([ids (or (attribute new-ids) #''())])
       #`(let ([res-info (phase1-eval (run-core-test p1 p2 ids))])
           (with-check-info*
             (list #,(gen-location-info stx))
             (λ () (check-true (car res-info) (cdr res-info))))))]))

(define-syntax (core-progs-not-equal? stx)
  (syntax-parse stx
    [(_ p1 p2 (~optional (~seq #:new-ids new-ids)))
     (with-syntax ([ids (or (attribute new-ids) #''())])
       #`(let ([res-info (phase1-eval (run-core-test p1 p2 ids))])
           (with-check-info*
             (list #,(gen-location-info stx))
             (λ () (check-false (car res-info) (cdr res-info))))))]))

(module+ test
  (require rackunit)

  (progs-not-equal? (generate-prog (a a))
                    (generate-prog (c c)))


  (progs-equal? (generate-prog ((~binder a) a))
                (generate-prog ((~binder c) c)))

  ;; NOTE this test _assumes_ that 'probably-unbound' is unbound (surprisingly)
  (progs-not-equal? (generate-prog (probably-unbound probably-unbound))
                    (generate-prog (probably-unbound probably-unbound)))

  (progs-equal? (generate-prog (car car))
                (generate-prog (car car)))

  (progs-equal? (generate-prog ((~binder a) a))
                #`(#,(syntax-property #'a 'binder #t) a))

  (progs-equal? (generate-prog ((~binder a) a))
                (list (syntax-property #'a 'binder #t) #'a))

  (progs-equal? (generate-prog ((~binders a b) ((~binder c) a)))
                (list* (syntax-property #'a 'binder #t)
                       (syntax-property #'b 'binder #t)
                       #`((#,(syntax-property #'c 'binder #t) a))))

  ;; testing alpha-equivalence of core forms
  (core-progs-equal? (generate-prog (define-values () 5))
                     (generate-prog (define-values () 5)))

  (core-progs-equal? (generate-prog (define-values (a) (+ 3 4)))
                     (generate-prog (define-values (c) (+ 3 4))))

  (core-progs-not-equal? (generate-prog (define-values (a b) (+ 3 4)))
                         (generate-prog (define-values (a) (+ 3 4))))

  ;; (core-progs-equal? (generate-prog (begin (define-values (a) (+ 3 4)) a))
  ;;                    (generate-prog (begin (define-values (c) (+ 3 4)) c))
  ;;                    #:new-ids (list #'a #'c))

  (core-progs-not-equal? (generate-prog (begin (define-values (a) (+ 3 4)) 5))
                         (generate-prog (begin (define-values (c) (+ 3 4)) c))
                         #:new-ids (list #'a #'c))

  ;; (core-progs-equal? (generate-prog (begin
  ;;                                     (define-values (a) (+ 3 4))
  ;;                                     (if a 5 6)))
  ;;                    (generate-prog (begin
  ;;                                     (define-values (z) (+ 3 4))
  ;;                                     (if z 5 6)))
  ;;                    #:new-ids (list #'a #'z))

  ;; (core-progs-equal? (generate-prog (begin (define-values (a b c) (+ 3 4)) c))
  ;;                    (generate-prog (begin (define-values (x y z) (+ 3 4)) z))
  ;;                    #:new-ids (list #'c #'z))

  (core-progs-equal? (generate-prog (#%plain-lambda (x) x))
                     (generate-prog (#%plain-lambda (y) y)))

  (core-progs-equal? (generate-prog (#%plain-lambda (x y z) x))
                     (generate-prog (#%plain-lambda (a b c) a)))

  (core-progs-not-equal? (generate-prog (#%plain-lambda (x y) x))
                         (generate-prog (#%plain-lambda (a b c) a)))

  (begin-for-syntax
    (define first-case-lambda
      (generate-prog
        (case-lambda
          [() (define-values () 5) 5]
          [(a) a]
          [(a b c) (+ b c)]
          [(a . rst) rst]
          [all (cons 5 all)])))

    (define second-case-lambda
      (generate-prog
        (case-lambda
          [() (define-values () 5) 5]
          [(o) o]
          [(e r g) (+ r g)]
          [(f . foo) foo]
          [many (cons 5 many)]))))

  (core-progs-equal? first-case-lambda second-case-lambda)

  ;; (core-progs-equal? (generate-prog (begin (define x 5) x))
  ;;                    (generate-prog (begin (define y 5) y))
  ;;                    #:new-ids (list #'x #'y))

  (core-progs-not-equal? (generate-prog (begin 5))
                         (generate-prog (begin 5 (+ 1 2))))

  (core-progs-not-equal? (generate-prog (let-values () 5 6))
                         (generate-prog (let-values () 5)))

  (core-progs-not-equal? (generate-prog (let-values () 5))
                         (generate-prog (let-values ([(a) 5]) 5)))

  (core-progs-not-equal? (generate-prog (let-values ([(a) 5]) 5))
                         (generate-prog (let-values ([(a b) 5]) 5)))

  (core-progs-not-equal? (generate-prog (let-values ([(a) 5] [(b) 6]) a))
                         (generate-prog (let-values ([(a) 5]) a)))

  (core-progs-equal? (generate-prog (let-values ([(a) 5]) a))
                     (generate-prog (let-values ([(b) 5]) b)))

  (core-progs-equal?
    (generate-prog
      (let-values ([(a b c) 5]
                   [(x y z) 6])
        (- b x)
        (+ c y)))
    (generate-prog
      (let-values ([(t u v) 5]
                   [(o p q) 6])
        (- u o)
        (+ v p))))

  (core-progs-not-equal? (generate-prog (let-values ([(a) 6] [(b) 5]) b))
                         (generate-prog (let-values ([(x) 6] [(y) 5]) x)))

  (core-progs-not-equal? (generate-prog (letrec-values () 5 6))
                         (generate-prog (letrec-values () 5)))

  (core-progs-equal? (generate-prog (letrec-values () 5))
                     (generate-prog (letrec-values () 5)))

  (core-progs-not-equal? (generate-prog (letrec-values ([(a b) 5]) a))
                         (generate-prog (letrec-values ([(a) 5]) a)))

  (core-progs-not-equal? (generate-prog (letrec-values ([(a) 5] [(b) 6]) a))
                         (generate-prog (letrec-values ([(a) 5]) a)))

  (core-progs-equal? (generate-prog (letrec-values ([(a) 5] [(b) 6]) b))
                     (generate-prog (letrec-values ([(x) 5] [(y) 6]) y)))

  (core-progs-not-equal? (generate-prog (letrec-values ([(a) 5] [(b) 6]) b))
                         (generate-prog (letrec-values ([(x) 5] [(y) 6]) x)))

  (begin-for-syntax
    (define first-letrec-values
      (generate-prog
        (letrec-values ([(even?) (#%plain-lambda (n) (if (zero? n) #t (odd? (sub1 n))))]
                        [(odd?) (#%plain-lambda (n) (if (zero? n) #f (even? (sub1 n))))])
          (even? 10000))))

    (define second-letrec-values
      (generate-prog
        (letrec-values ([(foobar?) (#%plain-lambda (n) (if (zero? n) #t (barfoo? (sub1 n))))]
                        [(barfoo?) (#%plain-lambda (n) (if (zero? n) #f (foobar? (sub1 n))))])
          (foobar? 10000)))))

  (core-progs-equal? first-letrec-values second-letrec-values)

  (core-progs-equal? (generate-prog (let-values ([(a) 5]) (set! a 3)))
                     (generate-prog (let-values ([(x) 5]) (set! x 3))))

  (core-progs-not-equal? (generate-prog (let-values ([(a b) 5]) (set! a 3)))
                         (generate-prog (let-values ([(x y) 5]) (set! y 3))))

  (core-progs-equal? (generate-prog '5) (generate-prog '5))

  (core-progs-equal? (generate-prog (let-values ([(a) 5]) 'a))
                     (generate-prog (let-values ([(b) 5]) 'a)))

  (core-progs-not-equal? (generate-prog 'a) (generate-prog 'b))
  
  (core-progs-not-equal? (generate-prog (let-values ([(a) 5]) 'a))
                         (generate-prog (let-values ([(b) 5]) 'b)))

  (core-progs-equal? (generate-prog (with-continuation-mark 5 6 7))
                     (generate-prog (with-continuation-mark 5 6 7)))

  (core-progs-equal?
    (generate-prog
      (with-continuation-mark (let-values () 'hello-there)
                              (let-values ([(a) 5]) a)
                              (let-values ([(x) (values 3)]) (+ 3 x))))
    (generate-prog
      (with-continuation-mark (let-values () 'hello-there)
                              (let-values ([(b) 5]) b)
                              (let-values ([(y) (values 3)]) (+ 3 y)))))

  (core-progs-equal?
    (generate-prog (let-values ([(foo) 5] [(bar) 6]) (#%plain-app foo bar)))
    (generate-prog (let-values ([(a) 5] [(b) 6]) (#%plain-app a b))))

  (core-progs-not-equal? (generate-prog (#%plain-app 5 6)) (generate-prog (#%plain-app 5)))

  (core-progs-equal? (generate-prog (#%expression 5)) (generate-prog (#%expression 5)))

  (core-progs-not-equal? (generate-prog (#%expression 5)) (generate-prog (#%expression 6)))

  (core-progs-equal? (generate-prog (let-values ([(a b) (values 3 4)]) (#%expression (+ a b))))
                     (generate-prog (let-values ([(x y) (values 3 4)]) (#%expression (+ x y)))))

  (core-progs-equal? (generate-prog (let ([x 5]) x))
                     (generate-prog (let ([y 5]) y)))



  )
