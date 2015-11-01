;:*=======================
;:* interval calculus
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;:*=======================
;:* arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= 0 (upper-bound y))
          (= 0 (lower-bound y)))
      (error "divisor y must not span 0 at either end.")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;:*=======================
;:* center-width specification
(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;:*=======================
;:* center-percentage specification
(define (make-center-percent c p)
  (make-interval (- c (* p c))
                 (+ c (* p c))))

(define (percent i)
  (/ (width i) (center i)))

(define (mul-interval-small-percent x y)
  (make-center-percent (* (center x) (center y))
                       (+ (percent x) (percent y))))


;:*=======================
;:* application
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

