;:*=======================
;:* smoothing a function in signal processing.
(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(define (smooth-nth-fold f n)
  ((repeated smooth n) f))

