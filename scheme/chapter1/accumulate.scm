;:*=======================
;:* generalization of sum and product
;:* e.g. (accumulate + 0 f a b next) = (sum f a b next)
;:* recursive
(define (accumulate combiner null-value f a b next)
  (if (> a b)
      null-value
      (combiner (f a)
                (accumulate combiner null-value f (next a) b next))))


;:*=======================
;:* generalization of sum and product
;:* e.g. (accumulate + 0 f a b next) = (sum f a b next)
;:* iterative
(define (accumulate combiner null-value f a b next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (f a)))))
  (iter a null-value))

(define (identity a) a)


;:*=======================
;:* define of applications
(define (sum f a b next)
  (accumulate + 0 f a b next))

(define (product f a b next)
  (accumulate * 1 f a b next))

