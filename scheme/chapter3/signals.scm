(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;:*=======================
;:* application
(define (RC R C dt)
  (lambda (i v0)
    (add-stream (scale-stream i R)
                (integral (scale-stream i (/ 1 C)) v0 dt))))

(define (smooth stream)
  (let ((first (stream-car stream))
        (rest (stream-cdr stream)))
    (cons-stream
     (average first (stream-car rest))
     (smooth rest))))

;:*=======================
;:* a better integral
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

;:*=======================
;:* application
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)
