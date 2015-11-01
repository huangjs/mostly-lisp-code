;:*=======================
;:* easy to debug
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;:*=======================
;:* more efficient
(define make-point cons)
(define x-point car)
(define y-point cdr)

;:*=======================
;:* application
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))


