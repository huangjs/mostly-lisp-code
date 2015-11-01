;:*=======================
;:* hierarchical structures
(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else
         (+ (count-leaves (car t))
            (count-leaves (cdr t))))))

;:*=======================
;:* tree reverse
(define (deep-reverse t)
  (define (iter t result)
    (cond ((null? t) result)
          ((not (pair? t)) t)
          (else
           (iter (cdr t)
                 (cons (deep-reverse (car t)) result)))))
  (iter t '()))

;:*=======================
;:* flatten
(define (flatten t)
  (cond ((null? t) '())
        ((not (pair? t)) (list t))
        (else (append (flatten (car t))
                      (flatten (cdr t))))))

;:*=======================
;:* tree map
(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))
