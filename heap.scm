;; Heap
;;


(define (make-heap size less)

  (let ((data (make-vector size #f))
        (count 0)
        (less less))

    (define (valid-node? node)  (< node count))

    (define (data-size)         (vector-length data))
    (define (data-ref node)     (vector-ref data node))
    (define (data-set! node val)(vector-set! data node val))

    (define (data-swap i j)
      (for-each (lambda (idx val) (data-set! idx val))
                (list i j)
                (list (data-ref j) (data-ref i))))

    (define (parent node)       (quotient (1- node) 2))
    (define (left node)         (+ 1 (* 2 node)))
    (define (right node)        (+ 2 (* 2 node)))
    (define (last-node)         (1- count))

    (define (less? node1 node2)
      (cond ((not (valid-node? node1)) #f)
            ((not (valid-node? node2)) #t)
            (else (less (data-ref node1) (data-ref node2)))))

    (define (min-of node1 node2)
      (if (less? node1 node2) node1 node2))

    (define (heapify-up node)
      (if (positive? node)
          (let ((prnt (parent node)))
            (if (less? node prnt)
                (begin
                  (data-swap prnt node)
                  (heapify-up prnt))))))

    (define (heapify-down node)
      (let ((smaller (fold min-of node (list (left node) (right node)))))
        (if (not (= node smaller))
            (begin
              (data-swap node smaller)
              (heapify-down smaller)))))

    (define (add val)
      (data-set! count val)
      (set! count (1+ count))
      (heapify-up (last-node)))

    (define (get)
      (if (zero? count)
        #f
        (let ((res (data-ref 0)))
          (data-swap 0 (last-node))
          (set! count (1- count))
          (heapify-down 0)
          res)))

    (define (ref node) (and (valid-node? node) (data-ref node)))

    (define (top) (ref 0))

    (define (log2 n) (/ (log n) (log 2)))

    (define (height)
      (if (zero? count) 0
        (inexact->exact (round (ceiling (log2 (1+ count)))))))

    (define (width)
      (if (zero? count) 0
        (expt 2 (1- (height)))))

    (define (assign another-heap)
      (set! data  (vector-copy (another-heap 'data)))
      (set! count (another-heap 'count))
      (set! less  (another-heap 'less)))

    (letrec ((this
               (lambda (op . args)
                 (case op
                   ;; state variables
                   ((data)    data)
                   ((count)   count)
                   ((less)    less)

                   ;; operations
                   ((add)     (add (car args)) this)
                   ((get)     (get))
                   ((top)     (top))
                   ((ref)     (ref (car args)))
                   ((size)    (data-size))
                   ((height)  (height))
                   ((width)   (width))
                   ((assign)  (assign (car args)) this)
                   (else (error "bad op:" op))))))
      this)))


(define (heap-add h x) (h 'add x))
(define (heap-get h)   (h 'get))
(define (heap-top h)   (h 'top))
(define (heap-ref h i) (h 'ref i))
(define (heap-count h) (h 'count))
(define (heap-size h)  (h 'size))
(define (heap-height h)(h 'height))
(define (heap-width h) (h 'width))

(define (heap-copy h)  ((make-heap 0 #f) 'assign h))


;; Print heap h as binary tree.
;;
;; xxxx      yyyy     zzzz...
;; ^^^^          ^^^^^
;; |             |
;; |             elements-gap
;; element-width
;;
;; (both element-width and elements-gap are defined for the bottom-most
;; level).
(define (print-heap h element-width elements-gap)

  ; pad item on left/right sides (in turn) with spaces until it gets
  ; given width.
  (define (format-item e width)
    (let loop ((s    (format #f "~a" e))
               (left #t))
      (if (>= (string-length s) width) s
        (loop (if left
                (string-append " " s)
                (string-append s " "))
              (not left)))))

  ; number of elements on j-th level.
  (define (level-width j)
    (expt 2 j))

  ; index of first element on j-th level.
  (define (level-head j)
    (1- (expt 2 j)))

  ; print gap as series of spaces
  (define (print-gap gap)
    (format #t "~a" (format-item "" gap)))

  ; print element
  (define (print-element i)
    (let ((e (heap-ref h i)))
      (format #t "~a" (format-item (if e e "") element-width))))

  ; print heap level by level starting from top (level 0)
  (for-each
    (lambda (e) ; e is the following structure: (level (offset . gap))
                ; level -- level index;
                ; offset -- offset of _CENTER_ of first element on the level;
                ; gap -- gap between _CENTERS_ of elements on the level.
      (let* ((level  (car e))
             (first  (level-head level))
             (offset (caadr e))
             (gap    (cdadr e)))
        (print-gap offset) ; shift first element
        (for-each ; print all elements
          (lambda (i)
            (if (> i first)                      ; print gap between elements
              (print-gap (- gap element-width))) ; if it's not first element.
            (print-element i))
          (iota (level-width level) first)))
      (format #t "\n\n")) ; couple of empty lines between levels.

    (let* ((gap    (+ element-width elements-gap)) ; distance between _CENTERS_
                                                   ; of elements on last
                                                   ; (bottom-most) level.
           (height (heap-height h))
           (levels (iota height)))

      (zip levels ; 'zip' is to join level index and its offset/gap pair.
           (fold
             (lambda (level res)
               (let* ((x     (car res))
                      (offst (car x))
                      (gap   (cdr x)))
                 (cons (cons (+ offst (/ gap 2)) (* gap 2)) res)))

             (list (cons 0 gap)) ; offset of first element 
                                 ; and gap between elements of last level.

             (cdr levels)))))); all levels but the last one


(define *test-heap*  (make-heap 100 <))
(dotimes (n 25)      (heap-add *test-heap* n))
(define (test-print) (print-heap *test-heap* 2 2))


(define (random-heap count max-number)
  (fold
    (lambda (n h)
      (heap-add h n)
      h)
    (make-heap count <)
    (map (lambda (n) (random max-number)) (iota count))))


(define (heap->list h)
  (let ((h (heap-copy h)))
    (let loop ((res '()))
      (let ((x (heap-get h)))
        (if (not x) (reverse res)
          (loop (cons x res)))))))


(define (heap-valid? h)
  (sorted? (heap->list h) <))


;; end of file
;; vim: ts=4 sw=4
