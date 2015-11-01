;:*=======================
;:* Get the length of list x
(define (length x)
  (cond ((null? x) 0)
        (else (+ 1 (length (cdr x))))))

;:*=======================
;:* Reverse x and add before z. If z is nil, reverse x exactly.
(define (rev x z)
  (cond ((null? x) z)
        (else (rev (cdr x) (cons (car x) z)))))

;:*=======================
;:* append x before z. x must be a list.
(define (append x z)
  (cond ((null? x) z)
        (else (cons (car x) (append (cdr x) z)))))

;:*=======================
;:* x: -> f(x)
(define (map f x)
  (cond ((null? x) '())
        (else (cons (f (car x)) (map f (cdr x))))))

;:*=======================
;:* filter
(define (remove-if f x)
  (cond ((null? x) '())
        ((f (car x)) (remove-if f (cdr x)))
        (else (cons (car x) (remove-if f (cdr x))))))

;:*=======================
;:* recursive pattern which can implement Sum, Multiply, etc.
(define (reduce f x v)
  (cond ((null? x) v)
        (else (f (car x) (reduce f (cdr x) v)))))

;:*=======================
;:* list the leaves from left to right
(define (flatten x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (flatten (car x))
                      (flatten (cdr x))))))

(define (square x) (* x x))
