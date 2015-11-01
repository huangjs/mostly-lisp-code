(define ones (cons-stream 1 ones))

(define (add-stream s1 s2)
  (stream-map + s1 s2))

;:*=======================
;:* magic! and BELIEF!
(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream 2
               (stream-filter prime? (integers-starting-from 3))))

;:*=======================
;:* this relies on the fact that p[n+1] <= p[n]^2 !!
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1
               (mul-streams factorials
                            (integers-starting-from 2))))

(define partial-sum
  (cons-stream 1
               (add-streams partial-sum
                            (integers-starting-from 2))))

;:*=======================
;:* merge!
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (cons-stream s2car
                                            (merge-weighted  (stream-cdr s1) (stream-cdr s2) weight)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))

(define (div-stream s1 s2)
  (stream-map / s1 s2))

(define (integrate-series stream)
  (div-stream stream
              integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define (neg-stream s)
  (stream-map - s))

(define cosine-series
  (cons-stream 1 (neg-stream (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;:*=======================
;:* polynomial multiplication s1 * s2
;:* (a0 + p)(b0 + q) = a0b0 + (a0q + b0p) + pq
;:* a0b0: 'const
;:* (a0q + b0p): 'series from x^1
;:* pq: 'series from x^2
(define (mul-series s1 s2)
  (let ((a0 (stream-car s1))
        (b0 (stream-car s2))
        (p (stream-cdr s1))
        (q (stream-cdr s2)))
    (cons-stream (* a0 b0)
                 (add-stream (cons-stream 0 (mul-series p q))  ;:* note: don't forget add 0!
                             (add-stream (scale-stream p b0)
                                         (scale-stream q a0))))))

(define (invert-series S)
  (let ((c (stream-car S))
        (SR (cons-stream 0 (stream-cdr S))))
    (define X
      (scale-stream (cons-stream 1 (neg-stream (mul-series SR X)))
                    (/ 1 c))))
  X)
    


;:*=======================
;:* sqrt-stream
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))


;:*=======================
;:* sequence accelerator
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

;:*=======================
;:* pi-stream
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define (partial-sums s)
  (add-stream s
              (cons-stream 0
                           (partial-sums s))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))



;:*=======================
;:* limited stream
(define (stream-limit s tolerance)
  (define (build-list list stream)
    (let ((s0 (stream-car stream))
          (s1 (stream-car (stream-cdr stream))))
      (if (< (abs (- s0 s1)) tolerance)
          (cons s0 list)
          (build-list (cons s0 list)
                      (stream-cdr stream)))))
  (reverse (build-list '() s)))

;:*=======================
;:* limited display
(define (display-n-stream s n)
  (if (= n 0)
      (newline)
      (begin (display (stream-car s))
             (display " ")
             (display-n-stream (stream-cdr s)
                               (- n 1)))))

