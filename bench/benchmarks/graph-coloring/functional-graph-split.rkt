#lang racket

(provide (all-defined-out))

(define (list-subtract l1 l2)
  ;; not the best algorithm
  (remove (lambda (elt) (member elt l2)) l1))

(define (graph-list-neighbours node edges)
  (define (update neighbours edge)
    ;; Using cons here assumes the graph has no redundancy,
    ;; Replace with list-insert (from mergesort) if needed.
    (cond ((equal? node (car edge))
	   (cons (cadr edge) neighbours))
	  ((equal? node (cadr edge))
	   (cons (car edge) neighbours))
	  (else neighbours)))
  (let loop ((neighbours '()) (edges edges))
    (if (null? edges)
	neighbours
	(loop (update neighbours (car edges)) (cdr edges)))))

(define (subgraph-edges nodes edges)
  ;; This lets remove edges from a graph by passing in a new
  ;; smaller list of nodes.
  (filter (lambda (edge)
	    (and (member (car edge) nodes)
		 (member (cadr edge) nodes)))
	  edges))

(define (graph-reduction-split nodes edges)
  (let-values
      (((nodes-a nodes-b)
	(partition (lambda (node) (> (length (graph-list-neighbours node edges)) 3))
		   nodes)))
    (let ((edges-a (subgraph-edges nodes-a edges)))
      (values nodes-a edges-a
	      nodes-b (list-subtract edges edges-a)))))

(define (graph-reduction-loop nodes edges tail)
  (let-values (((nodes-a edges-a nodes-b edges-b)
		(graph-reduction-split nodes edges)))
    (cond ((null? nodes-a)
	   ;; the graph is fully reduced (to the null graph)
	   (cons (cons nodes-b edges-b) tail))
	  ((null? nodes-b)
	   ;; the graph is irreducible
	   (cons (cons nodes-a edges-a) tail))
	  (else
	   (graph-reduction-loop nodes-a edges-a (cons (cons nodes-b edges-b) tail))))))

(define (graph-good-ordering nodes edges)
  (apply append (map car (graph-reduction-loop nodes edges '()))))
