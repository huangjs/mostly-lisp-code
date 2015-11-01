;:*=======================
;:* a^n mod m
(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n)
         (remainder (square (expmod a (/ n 2) m)) m))
        (else (remainder (* a (expmod a (- n 1) m)) m))))

(define (fermat-test n)
  (define a (+ (random (- n 1)) 1))
  (= (expmod a n n) a))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else (not #t))))

;:*=======================
;:* application
;:* try 5 times.
(define (prime? n)
  (if (< n 2)
      #f
      (fast-prime? n 5)))
