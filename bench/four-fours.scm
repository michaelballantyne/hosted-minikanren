(load "mk.scm")

(load "trace-define-mk.scm")

(trace-define-mk arithmetic
  (lambda (p r parse result)
    (fresh (q result1)
      (== p `(,result1 . ,q))
      (modify result1 result1 q r parse result))))

;; (trace-define-mk base
;;   (lambda (p q result1)
;;     ))

(trace-define-mk modify
  (lambda (parse1 result1 p r parse result)
    (conde [(== p r)
            (== parse1 parse)
            (== result1 result)]
           [(fresh (q parse2 result2 parse0 result0)
              (arithmetic p q parse2 result2)
              (conde [(== parse0 `(+ ,parse1 ,parse2))
                      (pluso result1 result2 result0)]
                     [(== parse0 `(- ,parse1 ,parse2))
                      (pluso result2 result0 result1)]
                     [(== parse0 `(* ,parse1 ,parse2))
                      (*o result1 result2 result0)]
                     [(== parse0 `(/ ,parse1 ,parse2))
                      (poso result2)
                      (*o result2 result0 result1)]
                     [(== parse0 `(sqrt ,parse))]
                     
                     )
              (modify parse0 result0 q r parse result))])))

(trace-define-mk four-fours
  (let ((p (map build-num '(4 4 4 4))))
    (lambda (n)
      (run 1 (parse)
           (arithmetic p '() parse (build-num n))))))


#| 

Suppose a grammar like:

```
E -> (E * E)
E -> (E / E)
E -> (E + E)
E -> (E - E)
E -> n 
```

First we left factor

```
E -> (E^ | n 
E^ -> E * E) | E / E) | E + E) | E - E) 
```

Rewrite to remove the indirect, then direct left recursions.
E < E^

```
 E -> n 
 E -> ( E^ 
E^ -> ( E^ * E )  
E^ -> ( E^ / E )  
E^ -> ( E^ + E )  
E^ -> ( E^ - E )  
E^ ->    n * E )  
```


|# 
