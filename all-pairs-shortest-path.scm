;; In this assignment you will implement one or more algorithms for the
;; all-pairs shortest-path problem. Here are data files describing three
;; graphs:
;; g1.txt
;; g2.txt
;; g3.txt
;; 
;; The first line indicates the number of vertices and edges, respectively.
;; Each subsequent line describes an edge (the first two numbers are its tail
;; and head, respectively) and its length (the third number). NOTE: some of the
;; edge lengths are negative. NOTE: These graphs may or may not have
;; negative-cost cycles.
;; 
;; Your task is to compute the "shortest shortest path". Precisely, you must
;; first identify which, if any, of the three graphs have no negative cycles.
;; For each such graph, you should compute all-pairs shortest paths and
;; remember the smallest one (i.e., compute
;;
;;                   min     d(u,v)
;;                      u,v∈V
;;
;; where d(u,v) denotes the shortest-path distance from u to v).
;; 
;; If each of the three graphs has a negative-cost cycle, then enter "NULL" in
;; the box below. If exactly one graph has no negative-cost cycles, then enter
;; the length of its shortest shortest path in the box below. If two or more of
;; the graphs have no negative-cost cycles, then enter the smallest of the
;; lengths of their shortest shortest paths in the box below.
;; 
;; OPTIONAL: You can use whatever algorithm you like to solve this question. If
;; you have extra time, try comparing the performance of different all-pairs
;; shortest-path algorithms!
;; 
;; OPTIONAL: Here is a bigger data set to play with.
;; large.txt
;; 
;; For fun, try computing the shortest shortest path of the graph in the file
;; above.


(load "bellman-ford.scm")
(load "dijkstra-shortest-path-by-heap.scm")


(define (extend-graph g count extra-node)
  (fold
    (lambda (node res)
      (cons
        (list extra-node node 0) ;; extra edge from extra node to every other nodes
        res))
    g
    (reverse (iota count 1))))


(define (johnson-all-pairs-shortest-path g)

  (let* (
         ;; number of nodes
         (n (graph-max-node g))

         ;; extra node for reweighting
         (extra-node (+ 1 n))

         ;; prepare reweighting
         ;; (Bellman-Ford Single Source Shortest Path)
         (w (bellman-ford-sssp (extend-graph g n extra-node) extra-node)))

    (if (not w)
      (begin
        ;; negative-cost cycle
        #f)
      (begin
        (let* ((p_ (list->vector (map cadr w)))
               (p  (lambda (node) (vector-ref p_ node)))

               (reweight-cost (lambda (cost from to)
                                (- (+ cost (p from)) (p to))))

               (restore-cost (lambda (cost from to)
                               (let ((p-from (p from))
                                     (p-to   (p to)))
                                 (if (and cost p-from p-to)
                                   (+ (- cost p-from) p-to)
                                   cost))))

               ;; reweighted graph
               (gr (map (lambda (e)
                          (let ((from (edge-from e))
                                (to   (edge-to e)))
                            ;;;(format #t "FIXME: e=~a\n" e)
                            (make-edge from to (reweight-cost (edge-cost e) from to))))
                        g))

               ;; best result
               (best-dist 0)
               (best-from 0)
               (best-to   0)

               (update-res!
                 (lambda (cost from to)
                   (if (< cost best-dist)
                     (begin
                       (format #t "FIXME: update res: ~a->~a\n" 
                               (list best-dist best-from best-to) (list cost from to))
                       (set! best-dist cost)
                       (set! best-from from)
                       (set! best-to to)))))

               ) ;; end of let

          (dotimes (s 1 n)
            (let (
                  ;; shortest path distantces from source node to all other nodes
                  ;; by Dijkstra
                  (sp (dijkstra-shortest-path gr s)))

              (format #t "FIXME: ~d/~d ~a\n" s n (list best-dist best-from best-to))

              ;; restore cost (reweight backwards)
              ;; and keep best result
              (dotimes (i 1 n)
                (if (not (= i s))
                  (let ((cost (vector-ref sp i)))
                    (if cost
                      (update-res! (restore-cost cost s i) s i)))))

          (list best-dist best-from best-to))))))))

;; -19
;; (g1 and g2 contain negative-cost cycles)
(define (g3)
  (johnson-all-pairs-shortest-path (read-graph-edges "g3.txt")))

(define (large)
  (johnson-all-pairs-shortest-path (read-graph-edges "large.txt")))


;; end of file
