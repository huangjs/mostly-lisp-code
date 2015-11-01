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

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position r k rest-of-queens)
  (cons (list r k) rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (define (same-line? p1 p2)
    (let ((r1 (car p1))
          (k1 (cadr p1))
          (r2 (car p2))
          (k2 (cadr p2)))
      (or (= r1 r2)
           (= (+ r1 k1)
              (+ r2 k2))
           (= (- r1 k1)
              (- r2 k2)))))
  (define (safe-simple? p1 p2)
    (not (same-line? p1 p2)))
  (let ((test-pos (car positions))
        (rest-pos (cdr positions)))
    (accumulate (lambda (a b)
                  (and a b))
                #t
                (map (lambda (p)
                       (safe-simple? p test-pos))
                     rest-pos))))

;(display (queens 8))
;(newline)
