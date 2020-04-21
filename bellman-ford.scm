;; Bellman-Ford algorithm to compute single source shortest path
;; with negative edge cost allowed.


(load "graph-utils.scm")


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


;; Bellman-Ford single source shortest path.
;; the 'g' is a list of edges, each edge is a '(from to cost)' triple.
;; the 's' is a source node (integer).
(define (bellman-ford-sssp g s)
  (let* (
         ;; number of nodes
         (n   (graph-max-node g))
         (n-1 (- n 1))

         ;; number of edges
         (m (length g))

         ;; infinity
         (inf (+ 1 (graph-max-cost g)))
         (res->cost (lambda (res) (if (or (not res) (>= res inf)) #f res)))

         ;; in-degree list of each node
         (in-degree_ (determine-in-degree g n))
         (in-degree  (lambda (v) (vector-ref in-degree_ v)))

         ;; two data lines for previous and current computations
         (A_         (make-vector 2 #f))
         (curr        0)
         (prev       #f)
         (shift-A    (lambda () (if (zero? curr)
                                  (begin
                                    (set! curr 1)
                                    (set! prev 0))
                                  (begin
                                    (set! curr 0)
                                    (set! prev 1)))))
         (A          (lambda (i v)
                       ;;;(format #t "FIXME: A ~a ~a\n" i v)
                       (vector-ref  (vector-ref A_ i) v)))
         (A!         (lambda (i v a) (vector-set! (vector-ref A_ i) v a)))

         ;; test for negative-cost cycles
         ;; (compare results of last and one extra iterations)
         (negative-cost-cycle?
           (lambda ()
             (let loop ((v 1))
               (if (> v n) #f
                 (if (not (= (A prev v) (A curr v))) #t
                   (loop (+ 1 v)))))))

         (improved? #t))

    ;; initialize data lines
    ;; [0..n] elements each
    (dotimes (x 2)
      (vector-set! A_ x (make-vector (+ 1 n) #f)))

    ;; set first data line
    (dotimes (v 0 n)
      (A! curr v (if (= v s) 0 inf)))

    (dotimes (i 1 n)
      (if improved?
        (begin
          (set! improved? #f)
          (shift-A)
          (if (zero? (remainder i 10))
            (format #t "i=~a\n" i))
          (dotimes (v 1 n)
            ;;;(format #t "i ~a v ~a\n" i v)
            (let ((aw (apply min inf (map (lambda (e)
                                            ;;;(format #t "FIXME: ~a\n" e)
                                            (let ((w   (edge-from e))
                                                  (Cwv (edge-cost e)))
                                              (+ (A prev w) Cwv)))
                                          (in-degree v)))))
              (if (< aw (A prev v))
                (begin
                  (set! improved? #t)
                  (A! curr v aw))
                (begin
                  (A! curr v (A prev v))))))
          (if (not improved?)
            (begin
              (format #t "early stop (~a)\n" i))))))

    (if (negative-cost-cycle?)
      (begin
        (format #t "negative-cost cycle\n")
        #f)
      (let ((res (make-vector (+ 1 n) #f)))
        (dotimes (v 0 n)
          (vector-set! res v (list v (res->cost (A prev v)))))
        (vector->list res)))))

;; end of file
