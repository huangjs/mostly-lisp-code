(define (pascal m n)
  (cond ((< m n) 0)
        ((< n 1) 0)
        ((= m 1) 1)
        (else (+ (pascal (- m 1)
                         (- n 1))
                 (pascal (- m 1)
                         n)))))

(define (pascal-row m)
  (if (< m 1)
      '()
      (pascal-gen m m)))

(define (pascal-gen m n)
  (if (= 1 n)
      (display 1)
      (lambda () ((display (pascal m n))
                  (pascal-gen m (- n 1))))))

