;:*=======================
;:* recursive
(define (cont-frac indexN indexD count)
  (define (cont-frac-help k)
    (if (> k count)
        0
        (/ (indexN k)
           (+ (indexD k)
              (cont-frac-help (+ k 1))))))
  (cont-frac-help 0))

;:*=======================
;:* iterative
(define (cont-frac-iter indexN indexD count result)
  (let ((new-result (/ (indexN (- count 1))
                       (+ (indexD (- count 1))
                          result))))
    (if (= count 0)
        result
        (cont-frac-iter indexN indexD (- count 1) new-result))))

(define (index-all-1 i)
  1.0)

;:*=======================
;:* Euler's natural number: e-2
(define (e-2 count)
  (define (indexD i)
    (if (= (remainder i 3) 2)
        (* 2
           (/ (+ i 1) 3))
        1))
  (cont-frac-iter index-all-1 indexD count 0.0))

