;:*=======================
;:* maybe memq is not needed. (already a primitive)
(define (memq x list)
  (cond ((null? list) #f)
        ((eq? x (car list))  #t)
        (else (memq x (cdr list)))))

(define (count-pairs x)
  (define pair-set '())
  (define (count-pairs-recur x)
    (cond ((not (pair? x)) 0)
          ((memq x pair-set) 0)
          (else
           (set! pair-set (cons x pair-set))
           (+ (count-pairs-recur (car x))
              (count-pairs-recur (cdr x))
              1))))
  (count-pairs-recur x))
