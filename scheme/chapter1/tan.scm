;:*=======================
;:* a continued fraction representation  by J.H.Lambert
(define (tan-cf x count)
  (define (tan-cf-recur k)
    (if (> k count)
        0
        (/ x
           (- (- (* 2 k) 1.0)
              (* x
                 (tan-cf-recur (+ k 1)))))))
  (tan-cf-recur 1))

;:*=======================
;:* iterative
(define (tan-cf-iter x count result)
  (let ((new-result (/ x
                       (- (- (* 2 count) 1)
                          (* x result)))))
    (if (= count 0)
        result
        (tan-cf-iter x (- count 1) new-result))))


                 
