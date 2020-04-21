;; Various utilities


(define (vector-update! v pos fn) (vector-set! v pos (fn (vector-ref v pos))))


;; end of file
