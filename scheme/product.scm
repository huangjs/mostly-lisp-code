(define (product nums)
  (define (loop nums acc)
    (if (null? nums)
        acc
        (loop (cdr nums) (* acc (car nums)))))
  (loop nums 1))

(define (product nums)
  (let loop ((nums nums) (acc 1))
    (if (null? nums)
        acc
        (loop (cdr nums) (* acc (car nums))))))

