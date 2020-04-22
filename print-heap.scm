


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

;; end of file
