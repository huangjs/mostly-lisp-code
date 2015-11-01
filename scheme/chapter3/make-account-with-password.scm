(define (make-account balance password)
  (define consecutive-incorrect-password-input-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (reset-cipi-count) (set! consecutive-incorrect-password-input-count 0))
  (define (call-the-cops) (error ":-("))
  (define (dispatch p m)
    (if (eq? p password)
        (begin
          (reset-cipi-count)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        (if (>= consecutive-incorrect-password-input-count 6)
            (call-the-cops)
            (begin
              (set! consecutive-incorrect-password-input-count
                    (+ consecutive-incorrect-password-input-count 1))
              (error "Incorrect password")))))
  dispatch)

(define (make-joint account password new-password)
  (define (guard input-password m)
    (if (eq? input-password new-password)
        (account password m)
        (error "Wrong password")))
  guard)
