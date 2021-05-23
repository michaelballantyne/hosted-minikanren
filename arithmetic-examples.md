


Solves `x^3 -12x + 16 = 0` for `x = 2`
```racket
(run 1 (q) 
  (fresh (b c d e)
	(fresh (a)	 
	  (expo q (build-num 3) a)
	  (*o a (build-num 1) b))
	(*o (build-num 12) q c)
	(pluso b (build-num 16) e)
	(minuso e c (build-num 0))))
```	
