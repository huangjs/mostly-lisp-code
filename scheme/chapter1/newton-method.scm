;:*=======================
;:* define derivative
(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx))))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))


;:*=======================
;:* application
(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x))
                 1.0))

