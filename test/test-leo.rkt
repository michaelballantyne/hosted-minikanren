#lang racket

(require "../main.rkt"
         syntax/parse)

(defrel/matche (leo x y)
  [(0 ,y)]
  [((S . ,x1) (S . ,y1))
   (leo x1 y1)])

#;(defrel (leo x y)
  (conde
   ((== x 0))
   ((fresh (x1 y1)
      (== x `(S . ,x1))
	  (== y `(S . ,y1))
	  (leo x1 y1)))))

;; (defrel (direct a b)
;;   (conde
;;     ((== a 'desplaines) (== b 'new-york-city))
;;     ((== a 'desmoinses) (== b 'losangeles))
;;     ((== a 'new-york-city) (== b 'losangeles))
;;     ((== a 'new-york-city) (== b 'desplaines))
;;     ((== a 'losangeles) (== b 'desmoinses))
;;     ((== a 'losangeles) (== b 'new-york-city))))


;; (defrel/matche (route-m origin destination path)
;;   [(,a ,a ())]
;;   [(,a ,b ((,a ,layover) . ,remaining-path))
;;    (absento a remaining-path)
;;    (direct a layover)
;;    (route-m layover b remaining-path)])

(define (reify-syntax-properties stx)
  (syntax-parse stx
    #:datum-literals (== #%lv-ref)
    [(== arg ...)
     #:when (syntax-property stx 'skip-occurs-check)
     #`(==/no-check . #,(map reify-syntax-properties (attribute arg)))]
    [(#%lv-ref v)
     #:when (syntax-property stx 'first-ref)
     #'(#%lv-ref/first v)]
    [(a . d) #`(#,(reify-syntax-properties #'a) . #,(reify-syntax-properties #'d))]
    [_ stx]))

(pretty-print (syntax->datum (relation-code leo)))
(pretty-print (map (λ (l) (list (first l) (syntax->datum (reify-syntax-properties (second l))))) (relation-code/optimized leo)))
(pretty-print (syntax->datum (relation-code/compiled leo)))
