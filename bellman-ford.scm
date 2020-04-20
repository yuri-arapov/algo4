;; Bellman-Ford algorithm to compute single source shortest path
;; with negative edge cost allowed.


(load "matrix.scm")


(define test-graph '(
  (1 2 2)
  (1 3 4)
  (2 3 1)
  (2 4 2)
  (3 5 4)
  (4 5 2)))


(define johnson-test-graph '(
  (1 2 -2)
  (2 3 -1)
  (3 1  4)
  (3 4  2)
  (3 5 -3)
  (6 4  1)
  (6 5 -4)))


(define negative-cycle-graph '(
  (1 2   1)
  (1 3  -1)
  (2 4   2)
  (2 5   4)
  (3 6   2)
  (4 1 -16)
  (4 6   3)
  (5 4   5)
  (6 5   4)))


(define (edge-from e) (car e))
(define (edge-to e)   (cadr e))
(define (edge-cost e) (caddr e))


(define (max-node g)
  (fold
    (lambda (e res) (max res (edge-from e) (edge-to e)))
    0
    g))


;; n - number of nodes
(define (determine-in-degree g n)
  (let ((x (make-vector (+ 1 n) '())))
    (for-each
      (lambda (e)
        (let* ((to        (edge-to   e))
               (in-degree (vector-ref x to)))
          (vector-set! x to (cons e in-degree))))
      g)
    x))


(define (graph-max-cost g)
  (fold
    (lambda (e res) (+ res (edge-cost e)))
    0
    g))


;; Bellman-Ford single source shortest path.
;; the 'g' is a list of edges, each edge is a '(from to cost)' triple.
;; the 's' is a source node (integer).
(define (bellman-ford-sssp g s)
  (let* (
         ;; number of nodes
         (n   (max-node g))
         (n-1 (- n 1))

         ;; number of edges
         (m (length g))

         (inf (+ 1 (graph-max-cost g)))
         (res->cost (lambda (res) (if (>= res inf) #f res)))

         ;; in-degree list of each node
         (in-degree_ (determine-in-degree g n))
         (in-degree  (lambda (v) (vector-ref in-degree_ v)))

         ;; 2-D array that keeps track of computations
         (A_         (make-matrix (+ 1 n) (+ 1 n) #f))
         (A          (lambda (i v)
                       ;;;(if (not (matrix-ref A_ i v)) (format #t "#f: i ~a v ~a\n" i v))
                       (matrix-ref A_ i v)))
         (A!         (lambda (i v a) (matrix-set! A_ i v a)))

         ;; test for negative cycles
         (negative-cycle
           (lambda ()
             (let loop ((v 1))
               (if (> v n) #f
                 (if (not (= (A n-1 v) (A n v))) #t
                   (loop (+ 1 v))))))))

    (dotimes (v 1 n) (A! 0 v (if (= v s) 0 inf)))

    (dotimes (i 1 n)
      (let ((i-1 (- i 1)))
        (dotimes (v 1 n)
          ;;;(format #t "i ~a v ~a\n" i v)
          (let ((aw (apply min inf (map (lambda (e)
                                          ;;;(format #t "~a\n" e)
                                          (let ((w   (edge-from e))
                                                (Cwv (edge-cost e)))
                                            (+ (A i-1 w) Cwv)))
                                        (in-degree v)))))
            (A! i v (min (A i-1 v) aw))))))

    (if (negative-cycle)
      #f
      (let ((res (make-vector (+ 1 n) #f)))
        (dotimes (v 1 n)
          (vector-set! res v (list v (res->cost (A n-1 v)))))
        (vector->list res)))))

;; end of file
