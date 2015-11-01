;:*=======================
;:* deprecated
(define (Fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (Fibonacci (- n 1))
                 (Fibonacci (- n 2))))))

;:*=======================
;:* iteration version
(define (Fibonacci n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

