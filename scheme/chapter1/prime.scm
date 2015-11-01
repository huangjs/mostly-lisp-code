(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test)
  (cond ((> (square test) n) n)
        ((divides? n test) test)
        (else (find-divisor n (+ 1 test)))))

;:*=======================
;:* a divides? b
(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= (smallest-divisor n) n))
