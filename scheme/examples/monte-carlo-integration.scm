(define (monte-carlo-integration trials experiment test-area)
  (* test-area
     (monte-carlo trials experiment)
     1.0))

(define (within-circle? x y)
  (<= (+ (square (- x 5))
         (square (- y 7)))
      (square 3)))

(define (gen-point)
  (cons (+ 2 (* (rand) 6))
        (+ 4 (* (rand) 6))))

(define (rand)
  (/ (random 1000000)
     1000000.0))

(define (circle-test)
  (let ((rand-point (gen-point)))
    (within-circle? (car rand-point)
                    (cdr rand-point))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
         
