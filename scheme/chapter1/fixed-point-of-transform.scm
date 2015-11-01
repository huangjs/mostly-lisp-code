;:*=======================
;:* very general procedure to fixed-point and newton-method
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;:*=======================
;:* application
(define (sqrt-fixed-point x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt-newton-method x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

