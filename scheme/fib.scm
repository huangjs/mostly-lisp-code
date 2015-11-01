(module fib
	(main main))

(define (main argv)
  (display (fib (string->number (car (cdr argv))))))

(define (fib::int n::int)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

