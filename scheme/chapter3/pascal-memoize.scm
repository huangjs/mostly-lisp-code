(define (pascal-triangle self row col)
  (cond ((and (= row 0) (= col 0)) 1)
        ((> col row) 0)
        ((< col 0) 0)
        (else
         (+ (self self (- row 1) col)
            (self self (- row 1) (- col 1))))))

(define (for start finish func)
  (if (< start finish)
      (begin
        (func start)
        (for (+ start 1) finish func))
      ()))

(define (test1 row col) (pascal-triangle pascal-triangle row col))

(define (triangle n)
  (for 0 n (lambda (max)
             (for 0 (+ max 1)
                  (lambda (x) (print (test1 max x)) (print " ")))
             (newline))))

(define (memoize func)
  (let ((memoresults '()))
    (lambda args
      (let ((memoized-pair (assoc args memoresults)))
        (if memoized-pair
            (cdr memoized-pair)
            (let ((thing-to-save (apply func args)))
              (begin
                (set! memoresults (cons (cons args thing-to-save) memoresults))
                thing-to-save)))))))

(define memo-pascal-triangle (memoize pascal-triangle))

(define (test2 row col) (memo-pascal-triangle memo-pascal-triangle row col))

(define (memo-triangle n)
  (for 0 n (lambda (max)
             (for 0 (+ max 1)
                  (lambda (x) (test2 max x)))
             )))
