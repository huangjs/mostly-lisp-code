;:*=======================
;:* an extremely general computational strategy
(define (iterative-improve good-enough? improve-method)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve-method) (improve-method guess)))))


;:*=======================
;:* application
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- x (square guess)))
       0.0001))
  (define improve-method
    (lambda (guess)
      (average guess (/ x guess))))
  ((iterative-improve good-enough? improve-method) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? x)
    (< (abs (- x (f x)))
       0.0001))
  (define improve-method
    (lambda (x)
      (f x)))
  ((iterative-improve good-enough? improve-method) first-guess))
