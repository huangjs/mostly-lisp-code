;:*=======================
;:* easy to debug
(define (make-segement x y) (cons x y))
(define (start-segement l) (car l))
(define (end-segement l) (cdr l))

;:*=======================
;:* more efficient
(define make-segement cons)
(define start-segement car)
(define end-segement cdr)

;:*=======================
;:* application
(define (midpoint-segement l)
  (make-point (average (x-point (start-segement l))
                       (x-point (end-segement l)))
              (average (y-point (start-segement l))
                       (y-point (end-segement l)))))

(define (print-segement l)
  (display "start point: ")
  (print-point (start-segement l))
  (display "end point: ")
  (print-point (end-segement l)))

(define (length-segement l)
  (sqrt (+ (square (- (x-point (start-segement l))
                      (x-point (end-segement l))))
           (square (- (y-point (start-segement l))
                      (y-point (end-segement l)))))))
