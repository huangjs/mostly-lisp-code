(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;:*=======================
;:* streams of pairs (diagonal)
;:* (pairs s t) => 
;:* (S0,T0) | (S0,T1) (S0,T2) ...
;;* --------|----------------------
;:*         | (S1,T1) (S1,T2) ...
;:*         |         (S2,T2) ...
;:*         |                 ...
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (pairs-test s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs-test (stream-cdr s) (stream-cdr t))))


;:*=======================
;:* applications
(define int-pairs (pairs integers integers))

(define prime-sum-pairs
  (stream-filter (lambda (pair)
                   (prime? (+ (car pair) (cadr pair))))
                 int-pairs))

(define (triples s t u)
  (cons-stream
   (list (stream-car s)
         (stream-car t)
         (stream-car u))
   (interleave
    (stream-map (lambda (x) (list (stream-car s)
                                  (stream-car t)
                                  x))
                (stream-cdr u))
    (interleave
     (stream-map (lambda (x) (cons (stream-car s) x))
                 (pairs (stream-cdr t) (stream-cdr u)))
     (triples (stream-cdr s)
              (stream-cdr t)
              (stream-cdr u))))))

(define int-triples (triples integers integers integers))

(define (pyth-weight pair)
  (+ (* (car pair) (car pair))
     (* (cadr pair) (cadr pair))))

(define pythagorean-triples
  (stream-filter (lambda (pair)
                   (= (pyth-weight pair)
                      (* (caddr pair) (caddr pair))))
                 int-triples))

;:*=======================
;:* wrong
(define (my-triples s t u)
  (interleave
   (stream-map (lambda (pair)
                 (cons (stream-car s) pair))
               (pairs t u))
   (my-triples (stream-cdr s)
               (stream-cdr t)
               (stream-cdr u))))

 
(define (quadras s t u v)
  (cons-stream
   (list (stream-car s)
         (stream-car t)
         (stream-car u)
         (stream-car v))
   (interleave
    (stream-map (lambda (point) (list (stream-car s)
                                      (stream-car t)
                                      (stream-car u)
                                      point))
                (stream-cdr v))
    (interleave
     (stream-map (lambda (pair) (append (list (stream-car s) (stream-car t))
                                        pair))
                 (pairs (stream-cdr u) (stream-cdr v)))
     (interleave
      (stream-map (lambda (triple) (cons (stream-car s)
                                         triple))
                  (triples (stream-cdr t) (stream-cdr u) (stream-cdr v)))
      (quadras (stream-cdr s) (stream-cdr t) (stream-cdr u) (stream-cdr v)))))))

;:*=======================
;:* addup-equals-ten
(define (quadras-addup-to quadras sum)
  (stream-filter (lambda (quadra)
                   (= sum
                      (+ (car quadra)
                         (cadr quadra)
                         (caddr quadra)
                         (cadddr quadra))))
                 quadras))

;:*=======================
;:* oddth and eventh indexed stream
(define (oddth-stream stream)
  (define (oddth-stream-help s odd?)
    (cond ((null? s) '())
          (odd?
           (cons-stream (stream-car s)
                        (oddth-stream-help (stream-cdr s) false)))
          (else
           (oddth-stream-help (stream-cdr s) true))))
  (oddth-stream-help stream true))

;:*=======================
;:* interesting result to (eventh-stream int-triples)
(define (eventh-stream stream)
  (define (eventh-stream-help s even?)
    (cond ((null? s) '())
          (even?
           (cons-stream (stream-car s)
                        (eventh-stream-help (stream-cdr s) false)))
          (else
           (eventh-stream-help (stream-cdr s) true))))
  (eventh-stream-help stream false))

;:*=======================
;:* weighted-pairs
(define (weighted-pairs s1 s2 w)
  (let ((s1car (stream-car s1))
        (s2car (stream-car s2))
        (s1cdr (stream-cdr s1))
        (s2cdr (stream-cdr s2)))
    (cons-stream (list s1car s2car)
                 (merge-weighted (stream-map (lambda (x) (list s1car x))
                                             s2cdr)
                                 (weighted-pairs s1cdr s2cdr w)
                                 w))))
  
;:*=======================
;:* Ramanujan numbers
(define (ram-weight pair)
  (+ (cube (car pair))
     (cube (cadr pair))))

(define ram-pairs
  (weighted-pairs integers integers ram-weight))

(define (delete-same op sorted-stream)
  (define (ds-help s p)
    (cond ((equal? (op p)
                   (op (stream-car s)))
           (ds-help (stream-cdr s) p))
          (else
           (cons-stream (stream-car s)
                        (ds-help (stream-cdr s) (stream-car s))))))
  (ds-help (stream-cdr sorted-stream)
           (stream-car sorted-stream)))

(define (find-same op sorted-stream)
  (define (fs-help s p)
    (cond ((equal? (op p)
                   (op (stream-car s)))
           (cons-stream p
                        (cons-stream (stream-car s)
                                     (fs-help (stream-cdr s) p))))
          (else
           (fs-help (stream-cdr s) (stream-car s)))))
  (fs-help (stream-cdr sorted-stream)
           (stream-car sorted-stream)))

;:*=======================
;:* pyth-pairs
(define pyth-pairs
  (weighted-pairs integers integers pyth-weight))
