;; Manually copied from Hemann’s ~/Documents/mkw-2022-paper/examples/
;; itself a modified and extended form of Michael and GregR’s
;; modification to Orchid’s graph-coloro from an old mKW hangout,
;; brought to my attention in an email.
;;
;; My own modifications to their program were to unroll the
;; higher-order goals back to first-order, and to add a whole boatload
;; of tests, including many other places and such.

(defrel (membero x l)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car))
	   ((membero x cdr)))))

(defrel (not-membero x l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (=/= x car)
	    (not-membero x cdr)))))

(defrel (appendo xs ys zs)
  (conde ((== xs '()) (== ys zs))
	 ((fresh (x-head x-tail z-tail)
	    (== xs `(,x-head . ,x-tail))
	    (== zs `(,x-head . ,z-tail))
	    (appendo x-tail ys z-tail)))))

(defrel (selecto x l l-x)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car)
	    (== l-x cdr))
	   ((fresh (cdr-x)
	      (== l-x `(,car . ,cdr-x))
	      (selecto x cdr cdr-x))))))

(defrel (mapo-coloro l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (coloro car)
	    (mapo-coloro cdr)))))

;; (defrel (mapo p l [acc (== 'cat 'cat)])
;;   (conde
;;    [(== l '())
;;     acc]
;;    [(fresh (car cdr)
;; 	   (== l `(,car . ,cdr))
;; 	   (mapo p cdr (fresh ()
;; 			      acc
;; 			      (p car))))]))

(defrel (mapo2-different-colors t l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (different-colors t car)
	    (mapo2-different-colors t cdr)))))

;; (defrel (mapo2 p t l [acc (== 'cat 'cat)])
;;   (conde
;;    [(== l '())
;;     acc]
;;    [(fresh (car cdr)
;; 	   (== l `(,car . ,cdr))
;; 	   (mapo2 p t cdr (fresh ()
;; 			      acc
;; 			      (p t car))))]))

(defrel (assoco key table value)
  (fresh (car table-cdr)
    (== table `(,car . ,table-cdr))
    (conde ((== `(,key . ,value) car))
	   ((assoco key table-cdr value)))))

(defrel (same-lengtho l1 l2)
  (conde ((== l1 '()) (== l1 '()))
	 ((fresh (car1 cdr1 car2 cdr2)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (same-lengtho cdr1 cdr2)))))

(defrel (make-assoc-tableo l1 l2 table)
  (conde ((== l1 '()) (== l1 '()) (== table '()))
	 ((fresh (car1 cdr1 car2 cdr2 cdr3)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (== table `((,car1 . ,car2) . ,cdr3))
	    (make-assoc-tableo cdr1 cdr2 cdr3)))))

(defrel (coloro x)
  (membero x '(red green blue yellow)))

(defrel (different-colors table constraint)
  (fresh (x y x-color y-color)
    (== constraint `(,x ,y))
    (assoco x table x-color)
    (assoco y table y-color)
    (=/= x-color y-color)))

;; (define (my-mapo p l i)
;;   ;; This has to be done in a depth first search!
;;   ;; (display (make-list i '-)) (newline)
;;   (conde/dfs ((== l '()))
;; 	     ((fresh (car cdr)
;; 		(== l `(,car . ,cdr))
;; 		(p car)
;; 		(my-mapo p cdr (+ i 1))))))

(defrel (color states edges colors)
  ;; This is a simple constrained generate and test solver
  ;; The interesting part was the graph reduction preprocessing
  ;; stage.
  (fresh (table)
    ;; make a list to hold the color of each state
    (make-assoc-tableo states colors table)

    ;; make sure each color is different to neighbours
    (mapo2-different-colors table edges)

    ;; brute force search for a valid coloring
    (mapo-coloro colors)))

(define (do-australia)
  (let ((nodes (graph-good-ordering australia:nodes australia:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes australia:edges q))))

(define (do-canada)
  (let ((nodes (graph-good-ordering canada:nodes canada:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes canada:edges q))))

(define (do-iberia)
  (let ((nodes (graph-good-ordering iberia:nodes iberia:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes iberia:edges q))))

(define (do-south-america)
  (let ((nodes (graph-good-ordering south-america:nodes south-america:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes south-america:edges q))))

(define (do-kazakhstan)
  (let ((nodes (graph-good-ordering kazakhstan:nodes kazakhstan:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes kazakhstan:edges q))))

(define (do-ireland)
  (let ((nodes (graph-good-ordering ireland:nodes ireland:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes ireland:edges q))))

(define (do-mexico)
  (let ((nodes (graph-good-ordering mexico:nodes mexico:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes mexico:edges q))))

(define (do-middle-earth)
  (let ((nodes (graph-good-ordering middle-earth:nodes middle-earth:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes middle-earth:edges q))))

(define (do-america)
  (let ((nodes (graph-good-ordering america:nodes america:edges)))
    ;; (display nodes)(newline)
    (run 1 (q) (color nodes america:edges q))))
