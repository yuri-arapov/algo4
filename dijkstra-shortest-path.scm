;; Dijkstra shortest graph path algorithm


(load "graph-utils.scm")


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

     ;; graph as adjacency list:
     ;; ((from1 (to1 cost1) (to2 cost2) ...) (from2 (to3 cost3)...) ...)
     (ag (graph-edges->adjacency-list g))

     ;; +infinity
     (inf #f)

     ;; tracking of minimum paths
     (_a (make-vector (+ 1 n) inf))
     (a  (lambda (v) (vector-ref _a v)))
     (a! (lambda (v dist) (vector-set! _a v dist)))
     (explored? (lambda (v) (a v)))

     ;; score structure: (distance from to)
     ;; "from" and "to" constitute an edge (tail and head)
     ;; "distance" - is a path distance from starting vertex to "to"
     (make-score (lambda (dist from to) (list dist from to)))
     (score-dist (lambda (s) (car s)))
     (score-from (lambda (s) (cadr s)))
     (score-to   (lambda (s) (caddr s)))

     (min-score (lambda (s1 s2)
                  (let ((d1 (score-dist s1))
                        (d2 (score-dist s2)))
                    (cond ((not d1) s2)
                          ((not d2) s1)
                          (else (if (< d1 d2) s1 s2))))))

     ;; ribs helpers
     (rib->score (lambda (from rib)
                   (make-score (+ (a from) (rib-cost rib)) from (rib-to rib))))
     (rib-explored? (lambda (rib) (explored? (rib-to rib))))

     ;; find edge with the best score that crosses the frontier.
     ;; return (score from to).
     (find-best-score
       (lambda ()
         (fold (lambda (node best-score)
                 ;;;(format #t "node=~a\n" node)
                 (let ((from (adj-node-from node)))
                   (if (explored? from)
                     (fold (lambda (rib best-score-so-far)
                             ;;;(format #t "rib=~a best-score-so-far=~a\n" rib best-score-so-far)
                             (if (not (rib-explored? rib))
                               (min-score (rib->score from rib) best-score-so-far)
                               best-score-so-far))
                           best-score
                           (adj-node-ribs node)) ;; throughout all node edges
                     best-score)))
               (make-score inf 0 0)
               ag)))) ;; throughout all graph adjacency nodes

    ;;;(format #t "nodes=~d edges=~d\n" n (length g))

    (a! s 0) ;; mark starting node

    (dotimes (i (- n 1))
      (let* ((bs   (find-best-score))
             (dist (score-dist bs))
             (from (score-from bs))
             (to   (score-to bs)))
        ;;;(format #t "bs=~a | s=~d (iter=~d) to=~d dist=~a\n" bs s i to dist)
        (a! to dist)))

    _a)) ;; result is a vector of shortest path distance from s to every other nodes


;; end of file
