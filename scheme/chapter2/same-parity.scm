;:*=======================
;:* the test of dotted-tail notation which can accept arbitrary numbers of arguments.
(define (same-parity a . items)
  (define (same-parity? b)
    (cond ((odd? a) (odd? b))
          ((even? a) (even? b))))
  (cons a (filter items same-parity?)))
