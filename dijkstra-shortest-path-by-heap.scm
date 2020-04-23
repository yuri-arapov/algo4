;; Dijkstra shortest graph path algorithm


(use-modules (srfi srfi-11)) ;; let-values

(load "graph-utils.scm")
(load "key-heap.scm")


(define dsp-test-graph1 '(
  (1 2 3)
  (1 3 2)
  (1 4 1)
  (2 3 1)
  (4 2 1)
  (4 3 5)))


;; Dijkstra shortest path (brute force)
;; g - graph as list of edges
;; s - starting vertex
(define (dijkstra-shortest-path g s)
  (let* (
     ;; number of nodes
     (n (graph-max-node g))

     ;; number of edges
     (m (length g))

     ;; heap containing edges that cross frontier
     (h (make-heap m <))

     ;; graph as adjacency vector
     ;; i-th vector element contains list of ribs the i-th node connected to
     (ag (graph-edges->adjacency-list g))

     (adj-list (lambda (node) (vector-ref ag node)))

     ;; tracking of minimum paths
     (_a (make-vector (+ 1 n) #f))
     (a  (lambda (v) (vector-ref _a v)))
     (a! (lambda (v dist) (vector-set! _a v dist)))
     (explored? (lambda (v) (a v)))

     (resolve-node
       (lambda (node distance)
         ;;(format #t "FIXME: resolve-node ~a,~a\n" node distance)
         (a! node distance)
         (for-each
           (lambda (rib)
             ;;(format #t "FIXME:   rib ~a\n" rib)
             (let ((head (rib-to rib)))
               (if (not (explored? head))
                 (if (heap-contains? h head)
                   (heap-update-key h head (min (heap-key h head) (+ distance (rib-cost rib))))
                   (heap-insert h head (+ distance (rib-cost rib)))
                   ))))
           (adj-list node))))

     ) ;; end of 'let'

    ;;;(format #t "nodes=~d edges=~d\n" n (length g))

    ;; add source node to solution
    (resolve-node s 0)

    (dotimes (i (- n 1))
      (let-values (((best-node best-cost) (heap-extract-v h)))
        (resolve-node best-node best-cost)))

    _a)) ;; result is a vector of shortest path distance from s to every other node


;; end of file
