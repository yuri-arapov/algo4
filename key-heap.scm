;; Heap


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

         (pos-key (lambda (pos) (node-key (pos-node pos))))

         (count 0)

         (log2 (lambda (n) (/ (log n) (log 2))))

         (height
           (lambda ()
             (if (zero? count) 0
               (inexact->exact (round (ceiling (log2 (1+ count))))))))

         (last (lambda () (- count 1)))

         (parent (lambda (pos) (quotient (- pos 1) 2)))
         (left   (lambda (pos) (+ 1 (* 2 pos))))
         (right  (lambda (pos) (+ 2 (* 2 pos))))

         (good-pos?
           (lambda (pos)
             (< pos count)))

         (min-of
           (lambda (p1 p2)
             (let ((k1 (and (good-pos? p1) (pos-key p1)))
                   (k2 (and (good-pos? p2) (pos-key p2))))
               (cond ((not k1) p2)
                     ((not k2) p1)
                     (else (if (less k1 k2) p1 p2))))))

         (less-by-pos?
           (lambda (p1 p2)
             (less (pos-key p1) (pos-key p2))))

         (swap
           (lambda (p1 p2)
             (let ((n1 (pos-node p1))
                   (n2 (pos-node p2)))
               (node-pos! n1 p2)
               (pos-node! p2 n1)
               (node-pos! n2 p1)
               (pos-node! p1 n2))))

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
               (error "already in the heap:" node))
             (let ((pos count))
               (set! count (+ 1 count))
               (node-key! node key)
               (node-pos! node pos)
               (pos-node! pos node)
               (heapify-up pos))))

         (extract-top
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

         (key
           (lambda (node)
             (if (or (negative? node) (>= node count))
               (error "out of range:" node))
             (node-key node)))

         (delete
           (lambda (node)
             (if (or (negative? node) (>= node count) (not (node-key node)))
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

         (modify
           (lambda (node key)
             (if (or (negative? node) (>= node count) (not (node-key node)))
               (error "not in heap:" node))
             (node-key! node key)
             (heapify (node-pos node))))

         (ref
           (lambda (pos)
             (if (not (good-pos? pos)) #f
               (list (pos-node pos) (pos-key pos)))))

         )

    (letrec ((this
               (lambda (op . args)
                 (case op
                   ((size)  size)
                   ((count) count)
                   ((height) (height))

                   ((insert) (insert (car args) (cadr args))) ;; node key

                   ((extract-top) (extract-top))

                   ((key) (key (car args))) ;; node

                   ((delete) (delete (car args))) ;; node

                   ((modify) (modify (car args) (cadr args))) ;; node key

                   ((ref) (ref (car args))) ;; pos

                   ))))

      this)))

(define (heap-size h)            (h 'size))
(define (heap-count h)           (h 'count))
(define (heap-height h)          (h 'height))
(define (heap-insert h node key) (h 'insert node key))
(define (heap-extract-top h)     (h 'extract-top))
(define (heap-delete h node)     (h 'delete node))
(define (heap-modify h node key) (h 'modify node key))
(define (heap-key h node)        (h 'key node))
(define (heap-ref h pos)         (h 'ref pos))

;; end of file
