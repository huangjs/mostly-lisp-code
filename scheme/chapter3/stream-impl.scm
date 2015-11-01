(define the-empty-stream '())

(define stream-null? null?)

(define (delay exp)
  (lambda () exp))

(define (force delayed-obj)
  (delayed-obj))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

;:*=======================
;:* better impl of delay using memoization
(define true #t)
(define false #f)

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay exp)
  (memo-proc (lambda ()  exp)))

