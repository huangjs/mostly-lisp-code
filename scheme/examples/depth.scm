(define (depth expr)
  (cond ((pair? x)
		 (+ 1 (apply max (map depth x))))
		(else 0)))
