;:*=======================
;:* filtered accumulation. Can be used as sum of prime numbers, etc.
;:* recursive
(define (filtered-accumulate combiner null-value f a b next filter)
  (define accumulate-help '(filtered-accumulate combiner null-value f (next a) b next filter))
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (f a)
                    accumulate-help)
          accumulate-help)))
                    

;:*=======================
;:* filtered accumulation. Can be used as sum of prime numbers, etc.
;:* iterative
(define (filtered-accumulate combiner null-value f a b next filter)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a)
                  (combiner (f a) result))
            (iter (next a) result))))
  (iter a null-value))

(define (identity a) a)

(define (non-filter a) #t)

;:*=======================
;:* applications
(define (filtered-sum f a b next filter)
  (filtered-accumulate + 0 f a b next filter))

(define (filtered-product f a b next filter)
  (filtered-accumulate * 1 f a b next filter))

