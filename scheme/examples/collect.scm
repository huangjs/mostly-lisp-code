

;:*=======================
;:* applications
(define (prime-sum-pairs n)
  (collect
   (list i j (+ i j))
   ((i (enum-interval 1 n))
    (j (enum-interval 1 (- i 1))))
   (prime? + i j)))
