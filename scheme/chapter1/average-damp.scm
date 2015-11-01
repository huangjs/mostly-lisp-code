;:*=======================
;:* [x+f(x)]/2
(define (average-damp f)
  (lambda (x) (average x (f x))))


;:*=======================
;:* application
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
