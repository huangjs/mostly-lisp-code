;:*=======================
;:* finding nth root of x to the nth.
(define (nth-root x n)
  (let ((repeats (floor (log2 n))))
    (fixed-point ((repeated average-damp repeats) (lambda (y) (/ x (fast-exp y (- n 1)))))
                 1.0)))
