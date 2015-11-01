;:*=======================
;:* product f(x) from a to b, with next(a) as next point.
;:* recursive
(define (product f a b next)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) b next))))

;:*=======================
;:* product f(x) from a to b, with next(a) as next point.
;:* iterative
(define (product f a b next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (f a)))))
  (iter a 1))

(define (identity a) a)
