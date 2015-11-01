;:*=======================
;:* (composition f g) =df f(g(x))
(define (compose f g)
  (lambda(x)
    (f (g x))))

;:*=======================
;:* application
(define (repeated f n)
  (if (<= n 0)
      identity
      (compose f (repeated f (- n 1)))))
