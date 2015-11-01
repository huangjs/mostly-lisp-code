;;; a simple counter
(let ((counter 0))
  (defun counter-next ()
    (incf counter))
  (defun counter-reset ()
    (setq counter 0)))

;;; be careful to ITERATIONS
; Closure captures assigned variable -- probably wrong 
(let ((fns ()))
  (dotimes (i 3)
    (push #'(lambda () i) fns))
  (mapcar #'funcall fns))
;; -> (3 3 3)
; New bindind created for each captured variable 
(let ((fns ()))
  (dotimes (i 3)
    (let ((i i))
      (push #'(lambda () i) fns)))
  (mapcar #'funcall fns))
;; -> (2 1 0)

;;; !! note the difference between (list ..) and '(..)
(defun stomp-a-constant ()
  (let ((l '(1 2 3))) ; compile-time constant data 
    (print l)
    (setf (second l) nil) ; destructive modification 
    l))

; ? (stomp-a-constant)
; (1 2 3)
; (1 NIL 3)
; ? (stomp-a-constant)
; (1 NIL 3)
; (1 NIL 3)
      
