;:*=======================
;:* both use procedure to represent data

;:*=======================
;:* dispatch version
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))


;:*=======================
;:* lambda version, like magic.
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


;:*=======================
;:* 2a3b impl
(define (cons a b)
  (* (fast-exp 2 a)
     (fast-exp 3 b)))

(define (car z)
  (factor z 2))

(define (cdr z)
  (factor z 3))
