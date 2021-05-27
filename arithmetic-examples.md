


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

```racket
(run 5 (pol)
  (fresh (q w x y z)
	(== q (build-num 3))
	(== pol `(,x * (x ^ ,w) - (,y * x) + ,z = 0 (x is ,q)))
	(poso x) 
	(>1o y) 
	(>1o w)
	(>1o q)
	(fresh (b c d e)
	  (fresh (a)	 
		(expo q w a)
		(*o a x b))
	  (*o y q c)
	  (pluso b z e)
	  (minuso e c (build-num 0)))))
```
returns
```
'(((1) * (x ^ (0 1)) - ((0 0 1) * x) + (1 1) = 0 (x is (1 1)))
  ((1) * (x ^ (0 1)) - ((0 0 0 1) * x) + (1 1 1 1) = 0 (x is (1 1)))
  ((1) * (x ^ (0 1)) - ((1 1) * x) + () = 0 (x is (1 1)))
  ((1) * (x ^ (0 1)) - ((0 0 0 0 1) * x) + (1 1 1 0 0 1) = 0 (x is (1 1)))
  ((0 1) * (x ^ (0 1)) - ((0 0 0 1) * x) + (0 1 1) = 0 (x is (1 1))))
```

3. Returns, hardly w/in a reasonable amount of time.

```
(run 6 (pol)
  (fresh (q w x y z)
	(== q (build-num 3))
	(== pol `(,x * (x ^ ,w) - (,y * x) + ,z = 0 (x is ,q)))
	(poso x) 
	(>1o y) 
	(>1o w)
	(>1o q)
	(fresh (b c d e)
	  (fresh (a)	 
		(expo q w a)
		(*o a x b))
	  (*o y q c)
	  (pluso b z e)
	  (minuso e c (build-num 0)))))
```

```
'(((1) * (x ^ (0 1)) - ((0 0 1) * x) + (1 1) = 0 (x is (1 1)))
  ((1) * (x ^ (0 1)) - ((0 0 0 1) * x) + (1 1 1 1) = 0 (x is (1 1)))
  ((1) * (x ^ (0 1)) - ((1 1) * x) + () = 0 (x is (1 1)))
  ((1) * (x ^ (0 1)) - ((0 0 0 0 1) * x) + (1 1 1 0 0 1) = 0 (x is (1 1)))
  ((0 1) * (x ^ (0 1)) - ((0 0 0 1) * x) + (0 1 1) = 0 (x is (1 1)))
  ((1) * (x ^ (0 1)) - ((0 1 1) * x) + (1 0 0 1) = 0 (x is (1 1))))
```





(run 6 (pol)
  (fresh (w x y z)
	(== pol `(,x * (x ^ ,w) - (,y * x) + ,z = 0 (x is ,(build-num 3))))
	(poso x) 
	(>1o y) 
	(>1o w)
	(fresh (b c d)
	  (fresh (a)	 
		(expo (build-num 3) w a)
		(*o a x b))
	  (*o (build-num 3) y c)
	  (pluso b z c))))
				   
				   a - b = 0 
