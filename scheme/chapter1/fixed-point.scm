;:*=======================
;:* finding fixed point of a function
(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
;      (display guess)
;      (newline)
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

;:*=======================
;:* application
;:* y^2 = x => y = x/y, which means we are trying to find a fixed point of function y -> x/y
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) ;otherwise it won't converge
               1.0))

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 
               1.0))

(define root-of-x^x=1000
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 
               2.0))
