;:*=======================
;:* search for 1 valid root
(define (search-root f neg-point pos-point)
  (define (close-enough? a b)
    (< (abs (- a b))
       0.0001))
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((= test-value 0) mid-point)
                ((> test-value 0) (search-root f neg-point mid-point))
                (else (search-root f mid-point pos-point)))))))


;:*=======================
;:* application
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search-root f a b))
          ((and (negative? b-value) (positive? a-value))
           (search-root f b a))
          (else (error "Values are not opposite sign" a b)))))
        
