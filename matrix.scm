;; simple matrix

(define (make-matrix columns rows init-value)
  (let ((m (make-vector (* columns rows) init-value)))
    (vector columns rows m)))

(define (matrix-ref m c r)
  (let ((cols (vector-ref m 0))
        (rows (vector-ref m 1))
        (data (vector-ref m 2)))
    (vector-ref data (+ (* cols r) c))))

(define (matrix-set! m c r value)
  (let ((cols (vector-ref m 0))
        (rows (vector-ref m 1))
        (data (vector-ref m 2)))
    (vector-set! data (+ (* cols r) c) value)))

;; end of file
