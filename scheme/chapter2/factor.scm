;:*=======================
;:* count the degree of a factor.
;:* n = x^factor
(define (factor n x)
  (if (= (remainder n x) 0)
      (+ 1
         (factor (/ n x) x))
      0))

