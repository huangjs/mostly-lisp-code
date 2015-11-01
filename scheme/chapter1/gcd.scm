(define (gcd a b)
  (if (> b a)
      (gcd-iter b a)
      (gcd-iter a b)))

(define (gcd-iter a b)
  (let ((r (remainder a b)))
    (if (= r 0)
        b
        (gcd-iter b r))))
                   
;:*=======================
;:* relative prime
(define (relative-prime? a b)
  (if (= (gcd a b) 1)
      #t
      #f))

