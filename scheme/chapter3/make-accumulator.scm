(define (make-accumulator addend)
  (lambda (augend)
    (begin (set! addend (+ addend augend))
           addend)))

