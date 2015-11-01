;:*=======================
;:* sum f(x) from a to b, with next(a) as next point.
;:* deprecated.
(define (sum f a b next)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) b next))))

;:*=======================
;:* sum f(x) from a to b, with next(a) as next point.
(define (sum f a b next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (f a)))))
  (iter a 0))

(define (identity a) a)

