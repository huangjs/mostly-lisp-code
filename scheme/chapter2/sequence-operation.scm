(define (filter predicate items)
  (cond ((null? items) '())
        ((predicate (car items))
         (cons (car items)
               (filter predicate (cdr items))))
        (else (filter predicate (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;(define (map proc items)
;  (if (null? items)
;      '()
;      (cons (proc (car items))
;            (map proc (cdr items)))))

;:*=======================
;:* enumerates
;:* output a sequence
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;:*=======================
;:* examples
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fibonacci
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              '()
              (map square
                   (map fibonacci
                        (enumerate-interval 0 n)))))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

;:*=======================
;:* yet another implementation
;:* MAGIC using accumulate!!!
;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x)
                                       (count-leaves x)
                                       1))
                       t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;:*=======================
;:* matrix
;:* NOTICE: using original map
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))


;:*=======================
;:* fold-left contrast to accumulation, which is known as fold-right
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;:*=======================
;:* application
(define (reverse sequence)
  (fold-right (lambda (x y) (cons y x)) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

