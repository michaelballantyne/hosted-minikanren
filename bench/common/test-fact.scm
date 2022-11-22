(defrel (fact n n!)
  (conde
   ((== n '()) (== n! '(1)))
   ((fresh (n-1)
      (pluso '(1) n-1 n)
      (fresh (n-1!)
        (fact n-1 n-1!)
        (*o n n-1! n!))))))

(defrel (fact1 n n!)
  (conde
   ((== n '()) (== n! '(1)))
   ((fresh (n-1)
      (pluso '(1) n-1 n)
      (fresh (n-1!)
        (*o n n-1! n!)
        (fact1 n-1 n-1!))))))

(define (fast-fact-7-5040)
  (run 1 (q) (fact q '(0 0 0 0 1 1 0 1 1 1 0 0 1))))

(define (slow-fact-7-5040)
  (run 1 (q) (fact1 q '(0 0 0 0 1 1 0 1 1 1 0 0 1))))
