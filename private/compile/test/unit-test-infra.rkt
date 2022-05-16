#lang racket

(require racket/syntax
         syntax/id-table
         syntax/stx
         syntax/macro-testing
         (for-syntax racket/syntax syntax/parse racket/base syntax/stx))

(provide alpha=?
         generate-prog
         get-test-result)

(module+ test
  (require rackunit
           syntax/macro-testing))

;; Represents test outcomes
(struct success ())
(struct failure (actual expected))

;; Return a failure result if either argument is a failure, otherwise return success
(define (combine res1 res2)
  (cond
    [(failure? res1) res1]
    [(failure? res2) res2]
    [else (success)]))

;; Produce a result object based on if the condition indicates success
(define (make-result c actual expected)
  (if c (success) (failure actual expected)))

(define get-status success?)

(define (get-error-message res)
  (if (success? res)
    ""
    (format "Actual does not equal expected. Actual: ~a. Expected: ~a"
            (syntax->datum #`#,(failure-actual res))
            (syntax->datum #`#,(failure-expected res)))))

;; NOTE a `cons` is returned here because this function is called inside
;;      `phase1-eval`, which expects one value, and because I haven't gotten
;;      accessors for a struct to work on an instance built at phase+1
(define (get-test-result res)
  (cons (get-status res) (get-error-message res)))


(define (alpha=? stx1 stx2)
  (alpha=?-helper stx1 stx2 (make-free-id-table)))

(define (alpha=?-helper stx1 stx2 table)
  (cond
    [(and (identifier? stx1)
          (identifier? stx2)
          (syntax-property stx1 'binder)
          (syntax-property stx2 'binder))
     (free-id-table-set! table stx1 stx2)
     (make-result #t stx1 stx2)]
    [(and (identifier? stx1)
          (identifier? stx2)
          (not (syntax-property stx1 'binder))
          (not (syntax-property stx2 'binder)))
     (let ([lookup (free-id-table-ref table stx1 #f)])
       (if lookup
         (make-result (free-identifier=? lookup stx2) stx1 stx2)
         (make-result (and (bound? stx1) (bound? stx2) (free-identifier=? stx1 stx2))
                      stx1
                      stx2)))]
    [(and (pair? stx1) (pair? stx2))
     (combine (alpha=?-helper (car stx1) (car stx2) table)
              (alpha=?-helper (cdr stx1) (cdr stx2) table))]
    [(and (syntax? stx1)
          (not (syntax? stx2)))
     (alpha=?-helper (syntax-e stx1) stx2 table)]
    [(and (not (syntax? stx1))
          (syntax? stx2))
     (alpha=?-helper stx1 (syntax-e stx2) table)]
    [(and (syntax? stx1)
          (syntax? stx2)
          (pair? (syntax-e stx1))
          (pair? (syntax-e stx2)))
     (let ([stx1-pair (syntax-e stx1)]
           [stx2-pair (syntax-e stx2)])
       (combine (alpha=?-helper (car stx1-pair) (car stx2-pair) table)
                (alpha=?-helper (cdr stx1-pair) (cdr stx2-pair) table)))]
    [(and (syntax? stx1) (syntax? stx2))
     (make-result (equal? (syntax->datum stx1) (syntax->datum stx2)) stx1 stx2)]
    [else (make-result (equal? stx1 stx2) stx1 stx2)]))


(module+ test
  (require syntax/parse/define)

  (define-syntax-parse-rule (check-failure tst) (check-pred failure? tst))
  (define-syntax-parse-rule (check-success tst) (check-pred success? tst))

  (check-failure (alpha=? (generate-prog (a a))
                          (generate-prog (c c))))

  (check-success (alpha=? (generate-prog ((~binder a) a))
                          (generate-prog ((~binder c) c))))

  ;; NOTE this test _assumes_ that 'probably-unbound' is unbound (surprisingly)
  (check-failure (alpha=? (generate-prog (probably-unbound probably-unbound))
                          (generate-prog (probably-unbound probably-unbound))))

  (check-success (alpha=? (generate-prog (car car))
                          (generate-prog (car car))))


  (define-syntax-rule (generate-expanded-prog template)
    (expand (generate-prog template)))

  (define something1
    (generate-expanded-prog
      (let ([(~binder a) 5])
        (let ([(~binder b) 6])
          a))))

  (define something2
    (generate-expanded-prog
      (let ([(~binder x) 5])
        (let ([(~binder y) 6])
          x))))

  (check-success (alpha=? something1 something2))

  (define something3
    (generate-expanded-prog
      (let ([(~binder x) 5])
        (let ([(~binder y) 6])
          y))))

  (check-failure (alpha=? something1 something3))

  )

(define (bound? id)
  (identifier-binding id (syntax-local-phase-level) #t))

(begin-for-syntax

  ;; Grammar for ~binder/~binders language:
  ;;
  ;; sexp := (~binder symbol)
  ;;       | (~prop sexp literal literal)
  ;;       | (~props sexp (~seq literal literal) ...+)
  ;;       | literal
  ;;       | list
  ;;
  ;; list := (cons (~binders symbol ...) list)
  ;;       | (cons sexp list)
  ;;       | literal
  ;;       | '()

  ;; Example of ~prop usage:
  ;; (generate-prog
  ;;   (ir-rel (foobaro (~binder a) (~binder b) (~binder c))
  ;;     (== (~prop (#%lv-ref a) 'foobar #t)
  ;;         (~prop (#%lv-ref b) 'foobar #f))))

  ;; return the identifiers in the contents of ~binder and ~binders forms
  ;; (-> syntax? (listof identifier?))
  ;; error cases include:
  ;; - malformed ~binder or ~binders forms
  ;; - no duplicate binders introduced
  (define (find-binders stx)
    (define (get-all-binders-sexp stx)
      (syntax-parse stx #:datum-literals (~binder ~binders ~prop ~props)
        [(~binder ~! b:id) (list #'b)]
        [(~prop ~! sexp key val) (get-all-binders-sexp #'sexp)]
        [(~props ~! sexp (~seq key val) ...+) (get-all-binders-sexp #'sexp)]
        [(~binders ~! b:id ...)
         (wrong-syntax this-syntax "~binders not allowed in this position")]
        [(_ . _) (get-all-binders-list this-syntax)]
        [_ '()]))

    (define (get-all-binders-list stx)
      (syntax-parse stx #:datum-literals (~binders)
        [((~binders ~! b:id ...) . d)
         (append (syntax->list #'(b ...)) (get-all-binders-list #'d))]
        [(a . d)
         (append (get-all-binders-sexp #'a) (get-all-binders-list #'d))]
        [_ '()]))

    (let* ([binders (get-all-binders-sexp stx)]
           [id? (check-duplicate-identifier binders)])
      (if id?
        (wrong-syntax id? "duplicate identifier introduced as binder")
        binders)))

  ;; replace the ~binder and ~binders forms with their contents and generate code that at runtime annotates each contained identifier with the syntax property 'binder
  ;; (-> syntax? syntax?)
  (define (strip-binders stx)
    (define (generate-annot id-arg)
      (with-syntax ([id id-arg])
        #'#,(mark-as-binder #'id)))

    (define (strip-binders-sexp stx)
      (syntax-parse stx #:datum-literals (~binder ~prop ~props)
        [(~binder b) (generate-annot #'b)]
        [(~prop sexp key val)
         (with-syntax ([stripped-sexp (strip-binders #'sexp)])
           #'#,(syntax-property #'stripped-sexp
                                (syntax->datum #'key)
                                (syntax->datum #'val)))]
        [(~props sexp (~seq key val) ...+)
         (with-syntax ([stripped-sexp (strip-binders #'sexp)]
                       [keys #'(key ...)]
                       [vals #'(val ...)])
           #'#,(apply-stx-props #'stripped-sexp #'keys #'vals))]
        [(_ . _) (strip-binders-list this-syntax)]
        [_ this-syntax]))

    (define (strip-binders-list stx)
      (syntax-parse stx #:datum-literals (~binders)
        [((~binders b ...) . d)
         (datum->syntax
           this-syntax
           (for/fold ([tail-e (strip-binders-list #'d)])
                     ([annotated-b (reverse (stx-map generate-annot #'(b ...)))])
             (cons annotated-b tail-e))
           this-syntax
           this-syntax)]
        [(a . d)
         (datum->syntax this-syntax
                        (cons (strip-binders #'a) (strip-binders #'d))
                        this-syntax
                        this-syntax)]
        [_ this-syntax]))

    (strip-binders-sexp stx))
  )

(define-syntax (generate-prog stx)
  (syntax-parse stx
    #:context stx
    [(_ body)
     (with-syntax ([(binder ...) (find-binders #'body)]
                   [template (strip-binders #'body)])
       #`(with-syntax ([(binder ...) (generate-temporaries #'(binder ...))])
           #`template))]))

(define (mark-as-binder id)
  (syntax-property id 'binder #t))

(define (apply-stx-props sexp keys vals)
  (for/fold ([sexp sexp])
            ([key (in-syntax keys)]
             [val (in-syntax vals)])
    (syntax-property sexp key val)))

(module+ test
  (define (make-a)
    (generate-prog (~binder a)))

  (check-true (identifier? (make-a)))
  (check-false (free-identifier=? (make-a) (make-a)))
  (check-true 
    (let ([prog (syntax->list (generate-prog ((~binder a) a)))])
      (free-identifier=? (car prog) (cadr prog))))

  (check-exn 
    #rx"^generate-prog: duplicate identifier introduced as binder$"
    (λ () (convert-syntax-error (generate-prog ((~binder a) (~binder a))))))

  (check-true (syntax-property (make-a) 'binder))

  (let* ([prog (syntax->list (generate-prog (((~binders a b c)) a)))]
         [binding-list (syntax->list (car prog))])
    (check-true (free-identifier=? (car binding-list) (cadr prog)))
    (check-equal? (length binding-list) 3))

  )

#|
(generate-prog
  (define-relation (appendo (~binders l1 l2 l3))
    (conde
      ((== l1 '()) (== l2 l3))
      ((fresh ((~binder a) (~binder d1) (~binder d2))
         (== l1 (cons a d1)))
         (== l3 (cons a d2))
         (appendo d1 l2 d2)))))

;; ----
(with-syntax ...
  #`(define-relation (appendo #,(syntax-property #'l1 'binder #t) ...)
      (conde
        ((== l1 ...))
        ((fresh (#,(syntax-property #'a 'binder #t) ...))))) ...)))

(generate-prog
  (define-relation (foo (~binder a))
    (== (~ref a) 5)))
;; -->
(with-syntax ([(a) (generate-temporaries #'(a))])
  #'(define-relation (foo a)
      (== a 5)))
(generate-prog
  (define-relation (foo (generate-temporarie


#'(define-relation (foo #<syntax a1>)
  (== #<syntax a1> 5))

(define (a-er)
  (generate-prog (~binder a)))

(bound-identifier=? (a-er) (a-er))

(generate-prog
  (define-relation (appendo (~binder l1 l2 l3))
    (conde
      ((== (~ref l1) '()) (== (~ref l2) (~ref l3)))
      ((fresh ((~binder a d1 d2))
         (== (~ref l1) (cons (~ref a) (~ref d1)))
         (== (~ref l3) (cons (~ref a) (~ref d2)))
         (appendo (~ref d1) (~ref l2) (~ref d2)))))))

(generate-prog
  (define-relation (appendo (~binders l1 l2 l3))
    (conde
      ((== l1 '()) (== l2 l3))
      ((fresh ((~binder a) (~binder d1) (~binder d2))
         (== l1 (cons a d1)))
         (== l3 (cons a d2))
         (appendo d1 l2 d2)))))



(generate-prog
  (define-relation (appendo l1#! l2#! l3#!)
    (conde
      ((== l1# '()) (== l2# l3#))
      ((fresh (a#! d1#! d2#!)
         (== l1# (cons a# d1#)))
         (== l3# (cons a# d2#))
         (appendo d1# l2# d2#)))))

(generate-prog
  (define-relation (appendo l1# l2# l3#)
    (conde
      ((== l1 '()) (== l2 l3))
      ((fresh (a# d1# d2#)
         (== l1 (cons a d1)))
         (== l3 (cons a d2))
         (appendo d1 l2 d2)))))

(generate-prog
  (let ([(~binder a) 5]
        [(~binder b) 6]
        [(~binder c) 7])
    (list a b c)))

(generate-prog
  (let ([a# 5]
        [b# 6]
        [c# 7])
    (list a b c)))

|#
