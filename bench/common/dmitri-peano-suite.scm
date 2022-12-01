;; Test suite from appendix to
;; A Complexity Study for Interleaving Search
;; Rozplokhas and Boulytchev

(defrel (leo x y)
  (conde
   ((== x 0))
   ((fresh (x1 y1)
      (== x `(S . ,x1))
	  (== y `(S . ,y1))
	  (leo x1 y1)))))

(defrel (pluso x y r)
  (conde
    ((== x 0) (== y r))
	((fresh (x1 r1)
	   (== x `(S . ,x1))
	   (== r `(S . ,r1))
	   (pluso x1 y r1)))))

(defrel (multo1 x y r)
  (conde
    ((== x 0) (== r 0))
	((fresh (x1 r1)
	   (== x `(S . ,x1))
	   (pluso r1 y r)
	   (multo1 x1 y r1)))))

(defrel (multo2 x y r)
  (conde
    ((== x 0) (== r 0))
	((fresh (x1 r1)
	   (== x `(S . ,x1))
	   (pluso r1 y r)
	   (multo2 x1 y r1)))))

(defrel (lengthdo a r)
  (conde
    ((== a '()) (== r 0)) ;; fixes bug in og appendix
	((fresh (h t r1)
	   (== a `(,h . ,t))
	   (lengthdo t r1)
	   (== r `(S . ,r1))))))

(defrel (lengtho a r)
  (conde
    ((== a '()) (== r 0)) ;; fixes bug in og appendix
	((fresh (h t r1)
	   (== a `(,h . ,t))
	   (== r `(S . ,r1))
	   (lengtho t r1)))))

(defrel (incr-listo a r)
  (conde
    ((== a '()) (== r '()))
	((fresh (h t tr)
	   (== a `(,h . ,t))
	   (== r `((S . ,h) . ,tr))
	   (incr-listo t tr)))))

(defrel (appendo a b r)
  (conde
    ((== a '()) (== b r))
	((fresh (h t tb)
	   (== a `(,h . ,t))
	   (== r `(,h . ,tb))
	   (appendo t b tb)))))

(defrel (reverseo1 a r)
  (conde
    ((== a '()) (== r '()))
	((fresh (h t tr) ;; fixes bug in og appendix
	   (== a `(,h . ,t))
	   (reverseo1 t tr)
	   (appendo tr `(,h . ()) r)))))

(defrel (reverseo2 a r)
  (conde
    ((== a '()) (== r '()))
	((fresh (h t tr) ;; fixes bug in og appendix
	   (== a `(,h . ,t))
	   (appendo tr `(,h . ()) r)
	   (reverseo2 t tr)))))

(define leo-801
  (let ((n (append (build-list 8000 (const 'S)) 0)))
	(lambda ()
      (run 1 (q) (leo n q)))))
