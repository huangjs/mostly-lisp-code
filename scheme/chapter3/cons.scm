;:*=======================
;:* Note: get-new-pair must be implemented by Lisp memory management
(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y))
  new)

