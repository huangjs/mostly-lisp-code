(define (timed-test func n)
  (newline)
  (display n)
  (start-test func n (current-milliseconds)))

(define (start-test func n start-time)
  (func n)
  (report-elapsed-time start-time (current-milliseconds)))

(define (report-elapsed-time start-time end-time)
  (display " *** ")
  (display (- end-time start-time))
  (display " ms ")
  (newline))
