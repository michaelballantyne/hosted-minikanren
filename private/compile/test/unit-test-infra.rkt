#lang racket

(require racket/syntax
         syntax/parse
         syntax/id-table
         syntax/stx
         syntax/macro-testing
         syntax/parse/define
         (for-syntax racket/sequence racket/syntax syntax/parse racket/base syntax/stx))

(provide alpha=?
         core/alpha=?
         generate-prog
         get-test-result)

;; TODO
;; 1. a macro that wraps or does the monadic stuff
;; 2. a macro that wraps return values & things with result objects
;; 3. a macro that handles the `for/list when lists are of equal length, but also fold, etc`
;; 4. a macro for abstracting over all of these for-loops that are annoying
;; 5. have a `set-id!` form that modifies the free-id-table and returns success
;; 6. macro for `combine` to effectively get monads -- combine will wrap each of its arguments
;;    in a thunk, then apply them one after the other until it receives failure, and will
;;    return that. it will otherwise return success.

;; Represents test outcomes
(struct success ())
(struct failure (actual expected))

(define (to-datum e)
  (if (syntax? e) (syntax->datum e) e))

;; Return a failure if any of the result structs are failures, otherwise succeed
(define (combine . reses)
  (or (findf failure? reses)
      (success)))

(define-syntax-parser diagnostic-syntax-parse
  [(_ stx kw litset [pat (~optional (~seq #:when cnd)) expr:expr ...+] ...+)
   #`(syntax-parse stx kw litset [pat (~? (~@ #:when cnd)) (printf "entered pattern ~a\n" 'pat) expr ...] ...)])


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

;; alpha-equivalence for IR tests
(define (alpha=? stx1 stx2)
  (alpha=?-helper stx1 stx2 (make-free-id-table)))

(define (core/alpha=? stx1 stx2 new-ids)
  (core/alpha=?-helper (local-expand stx1 'expression new-ids)
                       (local-expand stx2 'expression new-ids)
                       (make-free-id-table)))

(define (datum-length=? stx1 stx2)
  (= (length (to-datum stx1)) (length (to-datum stx2))))

(define (ids=? stx1 stx2 table)
  (let ([first-ids (syntax->list stx1)]
        [second-ids (syntax->list stx2)])
    (define len=? (datum-length=? first-ids second-ids))
    (when len=?
      (for ([id1 first-ids]
            [id2 second-ids])
        (free-id-table-set! table id1 id2)))

    (make-result len=? stx1 stx2)))

(define (formals=? f1 f2 table)
  (syntax-parse (list f1 f2) #:literal-sets (kernel-literals)
    [((id1:id ...) (id2:id ...)) (ids=? #'(id1 ...) #'(id2 ...) table)]
    [((id1:id ...+ . rest-id1:id)
      (id2:id ...+ . rest-id2:id))
     (ids=? #'(rest-id1 . (id1 ...)) #'(rest-id2 . (id2 ...)) table)]
    [(id1:id id2:id) (ids=? #'(id1) #'(id2) table)]))

;; ASSUMPTION: the free-id-table we're currently using will support alpha-equivalence
;; of core racket identifiers.
(define (core/alpha=?-helper stx1 stx2 table)
  (syntax-parse (list stx1 stx2) #:literal-sets (kernel-literals)

    ;; identifier case
    [(i1:id i2:id)
     (let ([lookup (free-id-table-ref table #'i1 #f)])
       (if lookup
         (make-result (free-identifier=? lookup #'i2) #'i1 #'i2)
         (make-result (and (bound? #'i1) (bound? #'i2) (free-identifier=? #'i1 #'i2))
                      #'i1
                      #'i2)))]

    ;; Racket core forms
    ;; Skipping (for now):
    ;; - module
    ;; - begin-for-syntax
    ;; - #%provide
    ;; - #%declare
    ;; - module*
    ;; - define-syntaxes
    ;; - #%require
    ;; - quote-syntax
    [((#%plain-lambda formals1 body1)
      (#%plain-lambda formals2 body2))
     (combine
       (formals=? #'formals1 #'formals2 table)
       (core/alpha=?-helper #'body1 #'body2 table))]
    [((case-lambda [formals1 body1 ...+] ...)
      (case-lambda [formals2 body2 ...+] ...))
     (apply combine
       (cons (make-result (datum-length=? #'(formals1 ...) #'(formals2 ...)) stx1 stx2)
         (for/list ([f1 (in-syntax #'(formals1 ...))]
                    [f2 (in-syntax #'(formals2 ...))]
                    [bodies1 (in-syntax #'((body1 ...) ...))]
                    [bodies2 (in-syntax #'((body2 ...) ...))])
           (let ([formals-res (formals=? f1 f2 table)])
             (apply combine
                (cons formals-res
                  (stx-map (λ (b1 b2) (core/alpha=?-helper b1 b2 table)) bodies1 bodies2)))))))]
    [((if c1 t1 e1) (if c2 t2 e2))
     (combine (core/alpha=?-helper #'c1 #'c2 table)
              (core/alpha=?-helper #'t1 #'t2 table)
              (core/alpha=?-helper #'e1 #'e2 table))]
    [(((~or* begin begin0) e1 ...)
      ((~or* begin begin0) e2 ...))
     (let ([len=? (datum-length=? #'(e1 ...) #'(e2 ...))])
       (if len=?
        (apply combine
                (stx-map (λ (e1 e2) (core/alpha=?-helper e1 e2 table))
                        #'(e1 ...)
                        #'(e2 ...)))
        (make-result #f stx1 stx2)))]

    [((let-values ([(id1:id ...) rhs1] ...) body1 ...+)
      (let-values ([(id2:id ...) rhs2] ...) body2 ...+))
     (if (and (datum-length=? #'(rhs1 ...) #'(rhs2 ...))
              (datum-length=? #'(body1 ...) #'(body2 ...)))
       (apply combine
         (append
           (for/list ([first-ids (in-syntax #'((id1 ...) ...))]
                      [second-ids (in-syntax #'((id2 ...) ...))]
                      [first-rhs (in-syntax #'(rhs1 ...))]
                      [second-rhs (in-syntax #'(rhs2 ...))])
             (combine (ids=? first-ids second-ids table)
                      (core/alpha=?-helper first-rhs second-rhs table)))
           (stx-map (λ (b1 b2) (core/alpha=?-helper b1 b2 table))
                    #'(body1 ...)
                    #'(body2 ...))))
       (make-result #f stx1 stx2))]

    [((letrec-values ([(id1:id ...) rhs1] ...) body1 ...+)
      (letrec-values ([(id2:id ...) rhs2] ...) body2 ...+))
     (if (and (datum-length=? #'(rhs1 ...) #'(rhs2 ...))
              (datum-length=? #'(body1 ...) #'(body2 ...)))
       (let ()
         (define id-reses
           (for/list ([first-ids (in-syntax #'((id1 ...) ...))]
                      [second-ids (in-syntax #'((id2 ...) ...))])
             (ids=? first-ids second-ids table)))

         (apply combine
           (append id-reses
             (for/list ([first-rhs (in-syntax #'(rhs1 ...))]
                        [second-rhs (in-syntax #'(rhs2 ...))])
               (core/alpha=?-helper first-rhs second-rhs table))
             (for/list ([first-body (in-syntax #'(body1 ...))]
                        [second-body (in-syntax #'(body2 ...))])
               (core/alpha=?-helper first-body second-body table)))))

       (make-result #f stx1 stx2))]

    [((set! id1:id e1)
      (set! id2:id e2))
     (combine (core/alpha=?-helper #'id1 #'id2 table)
              (core/alpha=?-helper #'e1 #'e2 table))]

    [((quote e1) (quote e2))
     (make-result (equal? (to-datum #'e1) (to-datum #'e2))
                  stx1
                  stx2)]
    [((with-continuation-mark k1 v1 e1)
      (with-continuation-mark k2 v2 e2))
     (combine (core/alpha=?-helper #'k1 #'k2 table)
              (core/alpha=?-helper #'v1 #'v2 table)
              (core/alpha=?-helper #'e1 #'e2 table))]

    [((#%plain-app e1 ...+)
      (#%plain-app e2 ...+))
     (if (datum-length=? #'(e1 ...) #'(e2 ...))
      (apply combine
        (for/list ([e1 (in-syntax #'(e1 ...))]
                   [e2 (in-syntax #'(e2 ...))])
          (core/alpha=?-helper e1 e2 table)))
      (make-result #f stx1 stx2))]
    
    [((#%top . id1:id) (#%top . id2:id)) (core/alpha=?-helper #'id1 #'id2 table)]
    [((#%variable-reference var1) (#%variable-reference var2))
     (core/alpha=?-helper #'var1 #'var2 table)]

    [((#%variable-reference) (#%variable-reference)) (make-result #t stx1 stx2)]

    [((#%expression e1) (#%expression e2)) (core/alpha=?-helper #'e1 #'e2 table)]

    [((define-values (id1 ...) b1)
      (define-values (id2 ...) b2))
     (let ([id-res (ids=? #'(id1 ...) #'(id2 ...) table)])
       (combine id-res (core/alpha=?-helper #'b1 #'b2 table)))]

    [_ (failure stx1 stx2)]))
      

(define (alpha=?-helper stx1 stx2 table)
  (syntax-parse (list stx1 stx2) #:literal-sets ()
    [_ #:when (syntax-property stx2 'check)
     (if (syntax-property stx1 (syntax-property stx2 'check))
       (alpha=?-helper stx1 (syntax-property-remove stx2 'check) table)
       (make-result #f stx1 stx2))]
    [_ #:when (syntax-property stx2 'missing)
     (if (syntax-property stx1 (syntax-property stx2 'missing))
       (make-result #f stx1 stx2)
       (alpha=?-helper stx1 (syntax-property-remove stx2 'missing) table))]
    [_ #:when (or (syntax-property stx1 'datum)
                  (syntax-property stx2 'datum))
     (make-result (equal? (syntax->datum stx1) (syntax->datum stx2))
                  stx1
                  stx2)]
    [(i1:id i2:id)
     #:when (and (syntax-property #'i1 'binder)
                 (syntax-property #'i2 'binder))
     (free-id-table-set! table stx1 stx2)
     (make-result #t stx1 stx2)]
    [(i1:id i2:id)
     #:when (not (or (syntax-property #'i1 'binder)
                     (syntax-property #'i2 'binder)))
     (let ([lookup (free-id-table-ref table #'i1 #f)])
       (if lookup
         (make-result (free-identifier=? lookup #'i2) #'i1 #'i2)
         (make-result (and (bound? #'i1) (bound? #'i2) (free-identifier=? #'i1 #'i2))
                      #'i1
                      #'i2)))]
    [((a1 . d1) (a2 . d2))
     (combine (alpha=?-helper #'a1 #'a2 table)
              (alpha=?-helper #'d1 #'d2 table))]
    ;; FIXME I don't like this branch
    [_
     (make-result (equal? (to-datum stx1) (to-datum stx2))
                  stx1
                  stx2)]))

(module+ test
  (require syntax/parse/define)

  (define-syntax-parse-rule (check-failure tst) (check-pred failure? tst))
  (define-syntax-parse-rule (check-success tst) (check-pred success? tst))

  ;; some direct alpha=? tests, majority of tests live in `unit-test-progs.rkt`
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
  ;; sexp := (~binder identifier)
  ;;       | (~prop sexp symbol literal)
  ;;       | (~props sexp (~seq literal literal) ...+)
  ;;       | (~check sexp symbol)
  ;;       | (~missing sexp symbol)
  ;;       | (~dat-lit sexp)
  ;;       | literal
  ;;       | list
  ;;
  ;; list := (cons (~binders identifier ...) list)
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
      (syntax-parse stx #:datum-literals (~binder ~binders ~check ~missing ~prop ~props ~dat-lit)
        [(~binder ~! b:id) (list #'b)]
        [(~check ~! sexp prop) (get-all-binders-sexp #'sexp)]
        [(~missing ~! sexp prop) (get-all-binders-sexp #'sexp)]
        [(~prop ~! sexp key val) (get-all-binders-sexp #'sexp)]
        [(~props ~! sexp (~seq key val) ...+) (get-all-binders-sexp #'sexp)]
        [(~dat-lit ~! sexp) (get-all-binders-sexp #'sexp)]
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

    (define (generate-datum-annot arg)
      (with-syntax ([stx arg])
        #'#,(mark-as-datum #'stx)))

    (define (strip-binders-sexp stx)
      (syntax-parse stx #:datum-literals (~binder ~check ~missing ~prop ~props ~dat-lit)
        [(~binder b) (generate-annot #'b)]
        [(~check sexp prop)
         (with-syntax ([stripped-sexp (strip-binders #'sexp)])
           #'#,(syntax-property #'stripped-sexp
                                'check
                                prop))]
        [(~missing sexp prop)
         (with-syntax ([stripped-sexp (strip-binders #'sexp)])
           #'#,(syntax-property #'stripped-sexp
                                'missing
                                prop))]
        [(~prop sexp key val)
         (with-syntax ([stripped-sexp (strip-binders #'sexp)])
           #'#,(syntax-property #'stripped-sexp
                                key
                                val))]
        [(~props sexp (~seq key val) ...+)
         (with-syntax ([stripped-sexp (strip-binders #'sexp)])
           #'#,(apply-stx-props #'stripped-sexp (list key ...) (list val ...)))]
        [(~dat-lit s) (generate-datum-annot #'s)]
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

(define (mark-as-datum id)
  (syntax-property id 'datum #t))

(define (apply-stx-props sexp keys vals)
  (for/fold ([sexp sexp])
            ([key (in-syntax keys)]
             [val (in-syntax vals)])
    (syntax-property sexp key val)))

(module+ test
  (require (except-in rackunit fail))

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
    (check-equal? (length binding-list) 3)))

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
