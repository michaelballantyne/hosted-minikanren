#lang racket

(require (except-in rackunit
                    fail)
         "../../main.rkt"
         "../../racket-matche.rkt")

;; Section 2

(defrel (direct a b)
  (conde
    [(== a 'BOS) (== b 'SEA)]
    [(== a 'HOU) (== b 'SLC)]
    [(== a 'SEA) (== b 'DEN)]
    [(== a 'SEA) (== b 'BOS)]
    [(== a 'DEN) (== b 'HOU)]
    [(== a 'SLC) (== b 'SFO)]))

(defrel (route origin end path)
  (conde
    [(== origin end) (== path '())]
    [(fresh (hop remainder)
       (== path (cons (list origin hop) remainder))
       (absento origin remainder)
       (direct origin hop)
       (route hop end remainder))]))

(check-equal?
  (run* (destination) (direct 'SEA destination))
  '(DEN BOS))

(check-equal?
  (run 3 (origin)
    (conde
      [(direct origin 'BOS)]
      [(direct origin 'HOU)]))
  '(SEA DEN))

(check-equal?
  (run* (lay1)
    (fresh (lay2)
      (direct 'DEN lay1) (direct lay1 lay2) (direct lay2 'SFO)))
  '(HOU))

(check-equal?
  (run* (path) (route 'BOS 'DEN path))
  '(((BOS SEA) (SEA DEN))))

;; 2.1

(require
 racket/list
 csv-reading
 net/url
 net/url-string)

(define SOURCE
  "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat")

;; () -> [Listof [Listof String String]]
;; Should produce a list of flights, each flight a list matching the data schema
(define (download-flights-csv)
  (for/list ([row (csv->list (get-pure-port (string->url SOURCE)))])
    (list (third row) (fifth row))))

(require (only-in "facts.rkt" define-facts-table query-facts))

(define-facts-table flights [flightfrom flightto]
  #:initial-data (download-flights-csv))

(check-match
  (run* (q) (query-facts flights "BOS" q))
  (list "ACK" "ALB" "AUG" _ ...))

(defrel (direct-db a b)
  (query-facts flights a b))

(defrel (route-m origin end path)
  (matche (origin end path)
    [(a a '())]
    [(a b (cons (list a layover) remainder))
     (absento a remainder)
     (direct-db a layover)
     (route-m layover b remainder)]))

;; St. Petersburg>Kazan
;; St. Petersburg>Nizhnekamsk(Begishevo)>Moscow>Kazan
(check-match
  (run 2 (q) (route-m "LED" "KZN" q))
  '((("LED" "KZN")) (("LED" "NBC") ("NBC" "DME") ("DME" "KZN"))))

;; Section 3

;; 3.3

(check-equal?
 (run 1 (x) (== (list (list 'a)) (cons (cons 'a '()) '())))
 (run 1 (x) (== (list x) (cons x '()))))

;; 4

;; 4.1

(defrel/matche (route-m-macro2 origin end path)
  [(a a '())]
  [(a b (cons (list a layover) remainder))
   (absento a remainder)
   (direct a layover)
   (route-m-macro2 layover b remainder)])

;; 4.2
(define (succeed/print str)
  (printf str)
  (expression-from-goal succeed))

(define (test-succeed/print)
  (pretty-print
    (run 1 (q)
      (fresh (x)
        (== q (list x 'cat))
        (goal-from-expression
          (succeed/print (format "value of ~a: ~a\n" 'q q)))))))

(check-equal?
 (with-output-to-string test-succeed/print)
 "value of q: (#<mk-lvar> cat)\n'((_.0 cat))\n")

(define (test-succeed/print-manual)
  (pretty-print
    (run 1 (q)
      (fresh (x)
        (== q (list x 'cat))
        (goal-from-expression
          (let ([str (format "value of ~a: ~a\n" 'q (expression-from-term q))])
            (succeed/print str)))))))

(check-equal?
 (with-output-to-string test-succeed/print-manual)
 "value of q: (#<mk-lvar> cat)\n'((_.0 cat))\n")

;; 4.3

;; 5

;; 5.2

(defrel/matche (leo x y)
  [('Z y)]
  [((cons 'S x1) (cons 'S y1)) (leo x1 y1)])

(check-equal?
  (run 10 (q) (leo q q))
  '(Z
    (S . Z)
    (S S . Z)
    (S S S . Z)
    (S S S S . Z)
    (S S S S S . Z)
    (S S S S S S . Z)
    (S S S S S S S . Z)
    (S S S S S S S S . Z)
    (S S S S S S S S S . Z)))
