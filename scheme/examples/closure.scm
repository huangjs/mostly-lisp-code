(define (create-add n)
    (lambda (k)
      (+ n k)))

(define (create-add2 n)
    (lambda (k)
      (set! n (+ n k))
      n))

