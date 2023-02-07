#lang racket/base

(require syntax/parse
         syntax/id-table
         racket/set
         racket/match
         racket/function
         racket/list
         (for-template racket/base
                       "../forms.rkt")
         "../syntax-classes.rkt")

(provide remove-no-escape/rel
         remove-no-escape/run)

;; I proceed under the assumption Michael already renamed-apart my
;; variables.
;;
;; For each variable, I have a structure.
;; 1. The set of ==s with j1 on the LHS (which can be more than 1)
;;
;; 2. The set of primitive goals with a reference to j1 in it *not* on
;; the LHS.
;;
;; 3. A mapping from each goal in 1. to the set
;;
;;

;; CAN A VARIABLE BE IN e IN APPLY RELATION?


;; gidtable -> [Listof Goal]
;; Excluding parameters, find all the ==  goals with a LHS var that are eliminable.
;; Operates to a fixpoint.
(define (discover-removables gid)

  (define-struct gid+removable (gid removable))

  ;; gidtable [Listof Goal] -> [Listof Goal]
  (define (discover-removables gid curr-removables)
    (match-define (goal-id-map params g->term-ids term-id->goals lhs->goals) gid)

    (define gid^+removable
      (for/first ([(id s) (in-free-id-table lhs->goals)]
                  #:when (eqv? 1 (set-count s))
                  #:do[(define g (set-first s))
                       (define goals-using (free-id-table-ref term-id->goals id))]
                  #:when (set-empty? goals-using))

        (define support (hash-ref g->term-ids g))

        (define new-term-id->goals
          (for/fold ([term-id->goals term-id->goals])
                    ([(tid __) (in-free-id-table support)])
            ;; Since we’ve removed goal g, g is no longer one of the
            ;; uses of any of the vars on the RHS of g
            (free-id-table-update term-id->goals tid (curryr set-remove g))))

        (gid+removable
         (goal-id-map
          params
          (hash-remove g->term-ids g)
          (free-id-table-remove new-term-id->goals id)
          (free-id-table-remove lhs->goals id))
         g)))

    (cond
      [gid^+removable
       (discover-removables
        (gid+removable-gid gid^+removable)
        (cons (gid+removable-removable gid^+removable) curr-removables))]
      [else curr-removables]))

  (discover-removables gid '()))

;; Table [Listof identifier] -> Table
(define (add-entries t ls v)
  (let ([entries-to-add (append-map (λ (id) (list id v)) ls)])
    (apply (curry free-id-table-set* t) entries-to-add)))

(define-struct goal-id-map (params g->term-ids term-id->goals lhs->goals) #:transparent)

;; listof id -> goalidtable
;; build an otherwise-empty goalidtable with the listed parameters
(define (make-goal-id-map-excluding params)
  (goal-id-map (add-entries (make-immutable-free-id-table) params #t)
               (make-immutable-hasheq)
               (make-immutable-free-id-table)
               (make-immutable-free-id-table)))

;; goalidtable listof id -> bool
;; add entries for each of these newly freshened variables in the given goalidtable
(define (add-fresh-vars gid xs)
  (match-define (goal-id-map params g->term-ids term-id->goals lhs->goals) gid)
  (goal-id-map
    params
    g->term-ids
    (add-entries term-id->goals xs (seteq))
    (add-entries lhs->goals xs (seteq))))

;; goalidtable id -> bool
;; decide if the given id is known as a parameter in the given goalidtable
(define (param? gid x)
  (match-define (goal-id-map params g->term-ids term-id->goals lhs->goals) gid)
  (free-id-table-ref params x #f))

(define (update-free-table/set-val t k v)
  (free-id-table-update t k (curryr set-add v)))

;; [goal#free-id-table] goal -> [goal#free-id-table]
;; In h, add id to the set of variables found in g, constructing the set if needed
(define (update-goal-table/set-val h g id)
  (hash-update
   h g
   (λ (idt) (free-id-table-set idt id #t))
   (λ () (make-immutable-free-id-table (list (cons id #t))))))

;; [goal#free-id-table] goal -> [goal#free-id-table]
;; Make sure there’s an entry for this goal in the goal -> ids hash
(define (update-goal-table/empty-set h g)
  (hash-update
   h g
   identity
   (λ () (make-immutable-free-id-table))))

;; goalidmap id goal -> goal
;; if x is not a parameter to the relation/run, add goal g to the set of goals that reference x as the LHS of an ==
(define (add-lhs gid x g)
  (match-define (goal-id-map params g->term-ids term-id->goals lhs->goals) gid)
  (cond
    [(param? gid x) gid]
    [else
     (goal-id-map
      params
      (update-goal-table/empty-set g->term-ids g)
      term-id->goals
      (update-free-table/set-val lhs->goals x g))]))

;; goalidmap id goal -> goalidmap
;; if x is not a parameter to the relation/run, add goal g to the set of goals that reference x not as a LHS of an ==
(define (add-term-id gid x g)
  (match-define (goal-id-map params g->term-ids term-id->goals lhs->goals) gid)
  (cond
    [(param? gid x) gid]
    [else
     (goal-id-map
      params
      (update-goal-table/set-val g->term-ids g x)
      (update-free-table/set-val term-id->goals x g)
      lhs->goals)]))

;; rel -> rel
;; Build a rel like the given rel, but substituting (success) for every no escape goal
(define (remove-no-escape/rel stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     (let ([goal-id-map (remove-no-escape/goal #'g (make-goal-id-map-excluding (attribute x)))])
       (let ([removable-goals (discover-removables goal-id-map)])
         (produce-remove-no-escape/rel stx removable-goals)))]))

;; run -> run
;; Build a run like the given run, but substituting (success) for every no escape goal
(define (remove-no-escape/run stx)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     (let ([goal-id-map (remove-no-escape/goal #'g (make-goal-id-map-excluding (attribute q)))])
       (let ([removable-goals (discover-removables goal-id-map)])
         (produce-remove-no-escape/run stx removable-goals)))]
    [(run* (q ...) g)
     (let ([goal-id-map (remove-no-escape/goal #'g (make-goal-id-map-excluding (attribute q)))])
       (let ([removable-goals (discover-removables goal-id-map)])
         (produce-remove-no-escape/run stx removable-goals)))]))

;; goal goalidmap -> goalidmap
;; Traverse, building the structure for atomic goals's variable references.
(define (remove-no-escape/goal g gidmap)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:nullary-constraint) gidmap]
    [(c:unary-constraint t)
     (remove-no-escape/term g #'t gidmap)]
    [(== (#%lv-ref v) t)
     (remove-no-escape/term g #'t (add-lhs gidmap #'v g))]
    [(== t1 t2) (raise-syntax-error "We should not be in this position")]
    [(c:binary-constraint t1 t2)
     (remove-no-escape/term g #'t1
       (remove-no-escape/term g #'t2 gidmap))]
    [(conj g1 g2)
     (remove-no-escape/goal #'g2
       (remove-no-escape/goal #'g1 gidmap))]
    [(disj g1 g2)
     ;; DEFICIENCY: treating the disj like a conj.
     (remove-no-escape/goal #'g2
       (remove-no-escape/goal #'g1 gidmap))]
    [(fresh (x ...) g)
     (remove-no-escape/goal
      #'g
      (add-fresh-vars gidmap (attribute x)))]
    [(#%rel-app n t ...)
     (foldl
      (curry remove-no-escape/term g)
      gidmap
      (attribute t))]
    [(apply-relation e t ...)
     (foldl
      (curry remove-no-escape/term g)
      gidmap
      (attribute t))]
))

;; goal term goalidmap -> goalidmap
;; Traverse this RHS term, building the structure for atomic goals'
;; variable references.
(define (remove-no-escape/term g t gid)
  (syntax-parse t
    #:literal-sets (mk-literals)
    #:literals (quote cons)
    [(#%term-datum l) gid]
    [(quote d) gid]
    [(rkt-term e) gid]
    [(#%lv-ref v)
     (add-term-id gid #'v g)]
    [(cons t1 t2)
     (remove-no-escape/term g #'t2
       (remove-no-escape/term g #'t1 gid))]))

;; rel [listof goal] -> rel
(define (produce-remove-no-escape/rel stx lrg)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(ir-rel (x ...) g)
     #`(ir-rel (x ...) #,(produce-remove-no-escape/goal #'g lrg))]))

;; run [listof goal] -> run
(define (produce-remove-no-escape/run stx lrg)
  (syntax-parse stx #:literal-sets (mk-literals)
    [(run n (q ...) g)
     #`(run n (q ...) #,(produce-remove-no-escape/goal #'g lrg))]
    [(run* (q ...) g)
     #`(run* (q ...) #,(produce-remove-no-escape/goal #'g lrg))]))

;; goal [listof goal] -> goal
(define (produce-remove-no-escape/goal g lrg)
  (syntax-parse g #:literal-sets (mk-literals)
    [(c:nullary-constraint) g]
    [(c:unary-constraint t) g]
    [(== (#%lv-ref v) t)
     (cond
       [(memq g lrg) #'(success)]
       [else g])]
    [(== t1 t2) (raise-syntax-error "We should not be in this position")]
    [(c:binary-constraint t1 t2) g]
    [(conj g1 g2)
     #`(conj #,(produce-remove-no-escape/goal #'g1 lrg)
             #,(produce-remove-no-escape/goal #'g2 lrg))]
    [(disj g1 g2)
     #`(disj #,(produce-remove-no-escape/goal #'g1 lrg)
             #,(produce-remove-no-escape/goal #'g2 lrg))]
    [(fresh (x ...) g)
     #`(fresh (x ...) #,(produce-remove-no-escape/goal #'g lrg))]
    [(#%rel-app n t ...) g]
    [(apply-relation e t ...) g]))

(module* test racket/base
  (require "./test/unit-test-progs.rkt"
           "../forms.rkt"
           rackunit
           (for-syntax racket/base
                       "./test/unit-test-progs.rkt"
                       (submod "..")))

  ;; Simple example of what pass should do
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binder a))
        (fresh ((~binder j))
          (== (#%lv-ref j) (#%lv-ref a))))))
    (generate-prog
     (ir-rel ((~binder a))
        (fresh ((~binder j))
          (success)))))

  ;; Example of what pass should do, w/cons RHS
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binders a d))
       (fresh ((~binder j))
         (== (#%lv-ref j) (cons (#%lv-ref a) (#%lv-ref d)))))))
  (generate-prog
    (ir-rel ((~binders a d))
      (fresh ((~binder j))
        (success)))))

  ;; Disjunctions before any conjs are just simple branches, treat simply
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binders a d))
      (fresh ((~binder j))
       (disj
        (== (#%lv-ref j) (cons (#%lv-ref a) (#%lv-ref d)))
        (== (#%lv-ref j) (cons (#%lv-ref a) (#%lv-ref d))))))))
    (generate-prog
     (ir-rel ((~binders a d))
      (fresh ((~binder j))
       (disj
        (success)
        (success))))))

  ;; This shows the kind of cascade we are wanting to capture all at
  ;; once rather than a 3-loop through algorithm.
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binder a))
      (fresh ((~binders j1 j2 j3))
       (conj
        (conj
         (== (#%lv-ref j1) (cons '5 (#%lv-ref a)))
         (== (#%lv-ref j3) (cons '8 (#%lv-ref j2))))
        (== (#%lv-ref j2) (cons '7 (#%lv-ref j1))))))))
    (generate-prog
     (ir-rel ((~binder a))
      (fresh ((~binders j1 j2 j3))
       (conj
        (conj
         (success)
         (success))
        (success))))))

  ;; This program, before constant folding, was the following.
  ;; Remember that constant folding will normalize away a whole lot
  ;; of possible problems.
  ;;
  ;; (ir-rel ((~binder a))
  ;;   (fresh ((~binders j1 j2 j3))
  ;;     (conj
  ;;       (conj
  ;;         (== (#%lv-ref j3) (cons (#%lv-ref a) (#%lv-ref j1)))
  ;;         (== (#%lv-ref j3) (cons (#%lv-ref a) (#%lv-ref j2))))
  ;;       (== (#%lv-ref j1) (#%lv-ref j2)))))
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binder a1))
       (fresh ((~binders j12 j23 j34))
         (conj
          (conj
           (== (#%lv-ref j34) (cons (#%lv-ref a1) (#%lv-ref j12)))
           (conj
            (success)
            (== (#%lv-ref j23) (#%lv-ref j12))))
          (success))))))
    (generate-prog
     (ir-rel ((~binder a1))
       (fresh ((~binders j12 j23 j34))
         (conj
          (conj
           (success)
           (conj
            (success)
            (success)))
          (success))))))

;; This was another one where, after folding, most of the difficulty
;; has simplified away.
(progs-equal?
  (remove-no-escape/rel
   (generate-prog
    (ir-rel ((~binders a b))
     (fresh ((~binders j1 j2 j3))
       (conj
        (conj
         (== (#%lv-ref j3) (cons (#%lv-ref a) (#%lv-ref b)))
         (conj
           (success)
           (== (#%lv-ref j1) (#%lv-ref b))))
        (success))))))
   (generate-prog
    (ir-rel ((~binders a b))
     (fresh ((~binders j1 j2 j3))
       (conj
        (conj
         (success)
         (conj
           (success)
           (success)))
        (success))))))

;; It’s not just the innermost fresh block at which we’d like remove
;; no-escape unifications
(let ((foo5 5))
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binder a))
      (fresh ((~binder x))
       (conj
        (fresh ((~binder j))
          (== (#%lv-ref j) (#%lv-ref x)))
        (== (#%lv-ref x) (#%lv-ref a)))))))
    (generate-prog
     (ir-rel ((~binder a))
      (fresh ((~binder x))
       (conj
        (fresh ((~binder j))
          (success))
        (success)))))))

;; We do no inlining of conses, and we do not in this pass look at
;; anything besides unifications. Removing disequalities would be
;; another pass.
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binder a))
       (fresh ((~binders j k))
         (conj
          (conj
           (== (#%lv-ref j) (cons (#%lv-ref a) '7))
           (== (#%lv-ref k) (cons (#%lv-ref a) '7)))
          (=/= (#%lv-ref j) (#%lv-ref k)))))))
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binders j k))
        (conj
         (conj
          (== (#%lv-ref j) (cons (#%lv-ref a) '7))
          (== (#%lv-ref k) (cons (#%lv-ref a) '7)))
         (=/= (#%lv-ref j) (#%lv-ref k)))))))

  ;; Repetitions of variables on RHS doesn’t matter for this pass,
  ;; there is no linearity restriction
(let ((foo8 5))
  (progs-equal?
   (remove-no-escape/rel
    (generate-prog
     (ir-rel ((~binders a))
       (fresh ((~binders j k))
         (== (#%lv-ref j) (cons (#%lv-ref k) (#%lv-ref k)))))))
   (generate-prog
    (ir-rel ((~binder a))
       (fresh ((~binders j k))
         (success))))))

  ;; We do not remove unifications where the LHS variable is a
  ;; parameter, b/c that’s external
  (let ((foo8 5))
    (progs-equal?
     (remove-no-escape/rel
      (generate-prog
       (ir-rel ((~binders a b))
         (fresh ((~binder j))
           (== (#%lv-ref a) (cons (#%lv-ref j) (#%lv-ref b)))))))
     (generate-prog
      (ir-rel ((~binders a b))
        (fresh ((~binder j))
          (== (#%lv-ref a) (cons (#%lv-ref j) (#%lv-ref b))))))))

;; An == where the LHS variable is later used *more than once*---in a
;; single relation call, in two relation calls, what have you---cannot
;; be removed.
;;
;; But Michael’s point was that we don’t *care* if we remove these or
;; not, because they’ll be hoisted to a let binding.
(let ((foo11 5))
 (progs-equal?
  (remove-no-escape/rel
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (== (#%lv-ref x) (cons (#%lv-ref j) '5))
            (#%rel-app foo11 (cons (#%lv-ref x) (#%lv-ref x)))))))))
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (== (#%lv-ref x) (cons (#%lv-ref j) '5))
            (#%rel-app foo11 (cons (#%lv-ref x) (#%lv-ref x))))))))))

;; But if you have only one use of a non-external variable other than
;; this ==, and that use of this variable comes after this
;; unification, then you can replace the unification by the RHS term,
;; *even* if it’s a cons, and *simultaneously* remove the unification.
;; BUT we wont to do that b/c the let lifting will do it enough for us.
(let ((foo9 5))
 (progs-equal?
  (remove-no-escape/rel
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (== (#%lv-ref x) (cons (#%lv-ref j) '5))
            (#%rel-app foo9 (#%lv-ref x))))))))
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (== (#%lv-ref x) (cons (#%lv-ref j) '5))
            (#%rel-app foo9 (#%lv-ref x)))))))))

;; Once some variable has been in a term in a relation call, it is
;; deemed external, and ==s with that var on the LHS are no longer
;; removable. But regardless b/c there’s another reference to it, its
;; not removable.
(let ((foo12 5))
 (progs-equal?
  (remove-no-escape/rel
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (#%rel-app foo12 (#%lv-ref x) (#%lv-ref a))
            (== (#%lv-ref x) (cons (#%lv-ref j) '5))))))))
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (#%rel-app foo12 (#%lv-ref x) (#%lv-ref a))
            (== (#%lv-ref x) (cons (#%lv-ref j) '5)))))))))

;; Even for variables in a term in a relation call, that are deemed
;; external, ==s with that var in the RHS are fine to remove if the
;; LHS variable is removable.
(let ((foo12a 5))
 (progs-equal?
  (remove-no-escape/rel
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (#%rel-app foo12a (#%lv-ref x) (#%lv-ref a))
            (== (#%lv-ref j) (cons (#%lv-ref x) '5))))))))
   (generate-prog
    (ir-rel ((~binder a))
      (fresh ((~binder x))
        (fresh ((~binder j))
          (conj
            (#%rel-app foo12a (#%lv-ref x) (#%lv-ref a))
            (success))))))))

;; Down a disj it’s removable if it’s removable in one of the
;; disjuncts.
(let ((foo13 5))
  (progs-equal?
    (remove-no-escape/rel
      (generate-prog
        (ir-rel ((~binder a))
          (fresh ((~binder x))
            (fresh ((~binder j))
              (conj
               (disj
                (== (#%lv-ref j) (#%lv-ref x))
                (#%rel-app foo13 (#%lv-ref j)))
                (#%rel-app foo13 (cons '7 (#%lv-ref a)))))))))
     (generate-prog
       (ir-rel ((~binder a))
         (fresh ((~binder x))
           (fresh ((~binder j))
             (conj
              (disj
               (== (#%lv-ref j) (#%lv-ref x))
               (#%rel-app foo13 (#%lv-ref j)))
               (#%rel-app foo13 (cons '7 (#%lv-ref a))))))))))

;; Dunno why this one was here. But it fits.
(let ((foo14 5))
  (progs-equal?
    (remove-no-escape/rel
     (generate-prog
       (ir-rel ((~binder a))
         (fresh ((~binder x))
           (fresh ((~binder j))
             (conj
              (conj
               (== (#%lv-ref x) (cons '7 '8))
               (== (#%lv-ref j) (cons (#%lv-ref x) (#%lv-ref x))))
               (#%rel-app foo14 (cons '7 (#%lv-ref x)))))))))
    (generate-prog
      (ir-rel ((~binder a))
        (fresh ((~binder x))
          (fresh ((~binder j))
            (conj
             (conj
              (== (#%lv-ref x) (cons '7 '8))
              (success))
              (#%rel-app foo14 (cons '7 (#%lv-ref x))))))))))

;; Only when there’s exactly one reference to that other variable do
;; we want to inline and remove the unification.
;; NOPE. We do not. Leave it alone.
(let ((foo15 5))
  (progs-equal?
    (remove-no-escape/rel
     (generate-prog
       (ir-rel ((~binder a))
         (fresh ((~binders j1 j2 j3))
             (conj
              (conj
               (conj
                (== (#%lv-ref j1) (cons '7 '8))
                (== (#%lv-ref j2) (cons '6 (#%lv-ref j1))))
               (== (#%lv-ref j3) (cons '5 (#%lv-ref j2))))
               (#%rel-app foo15 (cons (#%lv-ref j1) (cons (#%lv-ref j2) (#%lv-ref j3)))))))))
     (generate-prog
       (ir-rel ((~binder a))
         (fresh ((~binders j1 j2 j3))
             (conj
              (conj
               (conj
                (== (#%lv-ref j1) (cons '7 '8))
                (== (#%lv-ref j2) (cons '6 (#%lv-ref j1))))
               (== (#%lv-ref j3) (cons '5 (#%lv-ref j2))))
               (#%rel-app foo15 (cons (#%lv-ref j1) (cons (#%lv-ref j2) (#%lv-ref j3))))))))))


;; (fresh (j1)
;;        (conj
;;         (disj
;;          (== j1 (cons 5 6))
;;          (== j1 (cons 7 8)))
;;         (== j1 (cons 7 8))))

;; (fresh (j1)
;;        (conj
;;         (disj
;;          (== j1 (cons 5 6))
;;          (== j1 (cons 7 8)))
;;         (== j1 (cons 7 8))))


)

;; (fresh (j1)
;;        (conj
;;         (== j1 (cons 5 6))
;;         (disj
;;          (== cat cat)
;;          (success))))

;; (fresh (j1)
;;        (conj
;;         (== j1 (cons 5 6))
;;         (disj
;;          (foo j1)
;;          (success))))

;; (fresh (j1)
;;        (disj
;;         (conj
;;          (== j1 (cons 5 6))
;;          (foo j1))
;;         (success)))

;; (fresh (j1)
;;        (conj
;;         (disj
;;          (== j1 (cons 5 6))
;;          (== j1 (cons 7 8)))
;;         (== j1 (cons 7 8))))

;; ;;;;;
;; ;; When you know j1 will success
;; (fresh (j1 j2)
;;        (conj
;;         (conj
;;          (== j1 (cons 5 6))
;;          (bar j2))
;;         (disj
;;          (foo j1)
;;          (success))))
;; ;; ->
;; (fresh (j1 j2)
;;        (conj
;;         (bar j2)
;;         (disj
;;          (conj
;;           (== j1 (cons 5 6))
;;           (foo j1))
;;          (success))))
;; ;;;;;

;; (fresh (j1)
;;        (conj
;;         (== j1 (cons 5 6))
;;         (disj
;;          (conj
;;           (foo j1)
;;           (bar j1))
;;          (success))))

;; (fresh (j1)
;;        (conj
;;         (disj
;;          (foo j1)
;;          (success))
;;         (== j1 (cons 5 6))))
