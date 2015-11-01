;:*=======================
;:* the guy who invented lambda calculus.
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (add-1 zero))

(define two (add-1 one))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
