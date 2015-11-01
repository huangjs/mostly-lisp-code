(define (fast-exp b n)
  (cond ((= n 1) b)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (square (fast-exp b (/ (- n 1) 2)))))))

(define (square x)
  (* x x))
