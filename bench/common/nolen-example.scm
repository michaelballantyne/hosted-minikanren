
(defrel (infd1-10 x)
  (conde
    ((== x 1))
    ((== x 2))
    ((== x 3))
    ((== x 4))
    ((== x 5))
    ((== x 6))
    ((== x 7))
    ((== x 8))
    ((== x 9))
    ((== x 10))))

;; `all-in1-10` is just so that we can easily give each var a domain.

(defrel (all-in1-10 xs)
  (conde
    ((== xs '())
     (== 'cat 'cat))
    ((fresh (a d)
       (== xs `(,a . ,d))
       (infd1-10 a)
       (all-in1-10 d)))))

;; Note that in the following  we're simply asking for the first answer which should be list with 20 1's in.

(define (nolen-example)
  (let ((vs (reverse (build-list 20 identity))))
	(λ ()
	  (run 10 (q) (== q vs) (all-in1-10 vs)))))
