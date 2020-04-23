;; Heap
;;
;; Data element is a node (non-negative integer) and its key (number).
;;
;; Min-heap or max-heap depends on 'less' operator provide during
;; heap construction.
;;
;; Supports 'delete' operation.


(define (make-heap size less)

  (letrec* (
         ;; node->key mapping
         (node-key_ (make-vector size #f))
         (node-key  (lambda (node) (vector-ref node-key_ node)))
         (node-key! (lambda (node key) (vector-set! node-key_ node key)))

         ;; node->pos mapping
         (node-pos_ (make-vector size #f))
         (node-pos  (lambda (node) (vector-ref node-pos_ node)))
         (node-pos! (lambda (node pos) (vector-set! node-pos_ node pos)))

         ;; pos->node mapping
         (pos-node_ (make-vector size #f))
         (pos-node  (lambda (pos) (vector-ref pos-node_ pos)))
         (pos-node! (lambda (pos node) (vector-set! pos-node_ pos node)))

         ;; pos->key mapping
         (pos-key (lambda (pos) (node-key (pos-node pos))))

         ;; number of elemens in the heap
         (count 0)

         (node-in-range? (lambda (node) (< -1 node size)))

         (log2 (lambda (n) (/ (log n) (log 2))))

         (height
           (lambda ()
             (if (zero? count) 0
               (inexact->exact (round (ceiling (log2 (1+ count))))))))

         ;; last available position
         (last (lambda () (- count 1)))

         ;; parent and children of given position
         (parent (lambda (pos) (quotient (- pos 1) 2)))
         (left   (lambda (pos) (+ 1 (* 2 pos))))
         (right  (lambda (pos) (+ 2 (* 2 pos))))

         (good-pos?
           (lambda (pos)
             (< pos count)))

         ;; select position (p1 or p2) that corresponds to minimum key value
         (min-of
           (lambda (p1 p2)
             (let ((k1 (and (good-pos? p1) (pos-key p1)))
                   (k2 (and (good-pos? p2) (pos-key p2))))
               (cond ((not k1) p2)
                     ((not k2) p1)
                     (else (if (less k1 k2) p1 p2))))))

         ;; true if key from position p1 is less than key from position p2
         ;; false otherwise
         (less-by-pos?
           (lambda (p1 p2)
             (less (pos-key p1) (pos-key p2))))

         ;; exchange two elements addressed by their positions
         (swap
           (lambda (p1 p2)
             (let ((n1 (pos-node p1))
                   (n2 (pos-node p2)))
               (node-pos! n1 p2)
               (pos-node! p2 n1)
               (node-pos! n2 p1)
               (pos-node! p1 n2))))

         ;; true if given position has a parent
         (has-parent
           (lambda (pos)
             (positive? pos)))

         (heapify-up
           (lambda (pos)
             (let ((prnt (parent pos)))
               (if (and (has-parent pos) (less-by-pos? pos prnt))
                 (begin
                   (swap prnt pos)
                   (heapify-up prnt))))))

         (heapify-down
           (lambda (pos)
             (let ((smaller (fold min-of pos (list (left pos) (right pos)))))
               (if (not (= smaller pos))
                 (begin
                   (swap pos smaller)
                   (heapify-down smaller))))))

         (heapify
           (lambda (pos)
             (let ((prnt  (parent pos))
                   (left  (left pos))
                   (right (right pos)))
               (cond ((and (has-parent pos) (less-by-pos? pos prnt))
                      (swap pos prnt)
                      (heapify prnt))
                     ((and (good-pos? left) (less-by-pos? left pos))
                      (swap pos left)
                      (heapify left))
                     ((and (good-pos? right) (less-by-pos? right pos))
                      (swap pos right)
                      (heapify right))
                     (else
                       #t ;; heap property restored
                       )))))

         (insert
           (lambda (node key)
             ;;;(format #t "FIXME: insert ~d ~d\n" node key)
             (if (negative? node)
               (error "out of range:" node))
             (if (= count size)
               (error "heap is full"))
             (if (node-pos node)
               (error "already in heap:" node))
             (let ((pos count))
               (set! count (+ 1 count))
               (node-key! node key)
               (node-pos! node pos)
               (pos-node! pos node)
               (heapify-up pos))))

         (extract
           (lambda ()
             (if (zero? count)
               (error "heap is empty"))
             (let ((node (pos-node 0))
                   (key  (pos-key 0)))
               (swap 0 (last))
               (node-pos! node #f)
               (node-key! node #f)
               (set! count (- count 1))
               (heapify-down 0)
               (list node key))))

         ;; return key of given node
         (key
           (lambda (node)
             (if (not (node-in-range? node))
               (error "out of range:" node))
             (node-key node)))

         ;; delete node from heap
         (delete
           (lambda (node)
             (if (not (node-in-range? node))
               (error "out of range:" node))
             (if (not (node-key node))
               (error "not in heap:" node))
             (let ((pos (node-pos node))
                   (lst (last)))
               (if (= pos lst)
                 (begin
                   (set! count (- count 1))
                   (node-key! node #f)
                   (node-pos! node #f)
                   #t)
                 (begin
                   (swap pos lst)
                   (set! count (- count 1))
                   (node-key! node #f)
                   (node-pos! node #f)
                   (heapify pos))))))

         ;; replace key of given node
         (update-key
           (lambda (node key)
             (delete node)
             (insert node key)))

         ;; return (position key) tuple of given node
         (ref
           (lambda (pos)
             (if (not (good-pos? pos)) #f
               (list (pos-node pos) (pos-key pos)))))

         ;; return key of given node if heap contains one
         ;; return false otherwise
         (contains?
           (lambda (node)
             (and (node-in-range? node) (node-key node))))

         )

    (letrec ((this
               (lambda (op . args)
                 (case op
                   ((size)  size)  ;; capacity of the heap
                   ((count) count) ;; number of elements in the hreap
                   ((height) (height)) ;; height of the heap

                   ((insert) (insert (car args) (cadr args))) ;; node key
                   ((extract) (extract))
                   ((key) (key (car args))) ;; node
                   ((delete) (delete (car args))) ;; node
                   ((update-key) (update-key (car args) (cadr args))) ;; node key
                   ((ref) (ref (car args))) ;; pos
                   ((contains?) (contains? (car args))) ;; node

                   ((dump-keys) node-key_) ;; -> vector of keys
                   ))))
      this)))

(define (heap-size h)                (h 'size))                 ;; -> integer
(define (heap-count h)               (h 'count))                ;; -> integer
(define (heap-height h)              (h 'height))               ;; -> integer
(define (heap-insert h node key)     (h 'insert node key))      ;; -> undef
(define (heap-extract h)             (h 'extract))              ;; -> (node key)
(define (heap-extract-v h)           (apply values (h 'extract)));; -> (values node key)
(define (heap-delete h node)         (h 'delete node))          ;; -> #t
(define (heap-update-key h node key) (h 'update-key node key))  ;; -> undef
(define (heap-key h node)            (h 'key node))             ;; -> integer
(define (heap-ref h pos)             (h 'ref pos))              ;; -> (node key)
(define (heap-ref-v h pos)           (apply values (h 'ref pos)));; -> (values node key)
(define (heap-contains? h node)      (h 'contains? node))       ;; -> boolean

;; end of file
