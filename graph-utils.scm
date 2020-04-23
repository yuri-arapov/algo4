;; Graph utilites


(load "algo4-utils.scm")


;; edge is a '(from to cost)' triple
(define (make-edge from to cost) (list from to cost))

(define (edge-from e) (car e))
(define (edge-to e)   (cadr e))
(define (edge-cost e) (caddr e))


;; rib is an edge without 'from' node, so it's '(to cost)' pair
(define (make-rib to cost) (list to cost))

(define (rib-to a)   (car a))
(define (rib-cost a) (cadr a))


(define (edge->rib e) (make-rib (edge-to e) (edge-cost e)))


;; adj-node (adjacency node) consists of 'from' node and list of ribs
;; coming out of this node
(define (make-adj-node from rib-list)
  (if (null? rib-list)
    (list from)
    (cons from rib-list)))

(define (adj-node-from a) (car a))
(define (adj-node-ribs a) (cdr a))


;; g is a list of edges
(define (graph-max-cost g)
  (fold
    (lambda (e res) (+ res (abs (edge-cost e))))
    0
    g))


;; return list of edges
;; edge is a '(from to cost)' triple
(define (read-graph-edges filename)
  (map (lambda (line) (map string->number (string-split line #\space)))
       (cdr (read-file filename)))) ;; skip first line


;; g is a list of edges
;; return max node (number)
(define (graph-max-node g)
  (fold
    (lambda (e res) (max res (edge-from e) (edge-to e)))
    0
    g))


(define (graph-edges->adjacency-list g)
  (let ((x (make-vector (+ 1 (graph-max-node g)) '())))
    (for-each
      (lambda (e)
        (vector-update! x (edge-from e) (lambda (old) (cons (edge->rib e) old))))
      g)
    (dotimes (n (vector-length x))
      (vector-set! x n (reverse (vector-ref x n))))
    x))



;; end of file
