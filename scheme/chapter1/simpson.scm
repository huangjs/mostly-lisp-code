;:*=======================
;:* Simpson's rules for integration.
;:* integral f(x) from a to b = h/3 * {y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 4yn-1 + yn}
(define (simpson f a b n)
  (let ((dx (/ (- b a)
               n)))
    (define (add-dx x) (+ x dx))
    (define (add-2dx x) (+ x dx dx))
    (define sum-y (+ (sum f a b add-dx)
                     (sum f (+ a dx) (- b dx) add-dx)
                     (* 2 (sum f (+ a dx) (- b dx) add-2dx))))
    (/ (* dx
          sum-y)
       3)))



