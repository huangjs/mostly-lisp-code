(define random-init 4376821)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (remainder (+ (* 2137 x)
                8795)
             10000000))

;:*=======================
;:* objective rand
(define rand
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset)
      (set! x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) (reset))
            (else (error "Unknown message"))))
    dispatch))
