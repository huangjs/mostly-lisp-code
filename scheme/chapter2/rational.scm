;:*=======================
;:* define the rational class and its selector and constructor

;:*=======================
;:* helper tools, gcd iterative version
(define (gcd a b)
  (let ((r (remainder a b)))
    (if (= r 0)
        b
        (gcd b r))))

;:*=======================
;:* more efficient
(define make-rat cons)
(define numer car)
(define denom cdr)

;:*=======================
;:* easy for debug
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;:*=======================
;:* with gcd
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

;:*=======================
;:* arithmetic
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;:*=======================
;:* application
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
