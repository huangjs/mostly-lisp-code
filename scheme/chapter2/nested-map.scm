(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list '())    ;:* for flatmap's purpose
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter (lambda (p) (prime? (+ (car p)
                                      (cadr p))))
               (unique-pairs n))))

;:*=======================
;:* application
(define (all-triple-pairs-sum-to n)
  (flatmap (lambda (k)
             (map (lambda (j)
                    (let ((i (- n k j)))
                      (list i j k)))
                  (enumerate-interval 1 (- n k 1))))
           (enumerate-interval 1 n)))

