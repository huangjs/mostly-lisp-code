(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (double x)
  (+ x x))

(define (triple x)
  (+ x x x))

(define (inc x)
  (+ x 1))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (>= x 0)
      x
      (- x)))

(define (negative? x)
  (< x 0))

(define (positive? x)
  (> x 0))

(define (log2 x)
  (/ (log x)
     (log 2)))
