(define (inverter in out)
  (define (invert-in)
    (let ((new
           (logical net (get-signal in))))
      (after-delay inverter-delay
                   (lambda ()
                     (set signal! out new)))))
  (add-action! in invert-in))

;:*=======================
;:* FIXME! NOT COMPLETE
