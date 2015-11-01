;:*=======================
;:* global variable
(defvar *count* 0
  "Count of widgets made so far.")

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

(defun increment-widget-count ()
  (incf *count*))

;:*=======================
;:* assignment and scope
(defun foo (x) (setf x 10))

(defun experiment ()
  (let ((y 20))
    (foo y) ; -> 10
    (print y))) ; -> print 20!!!

;;; this is also allowed
;;; (setf x 1 y 2)
;;; (setf x (setf y (random 10)))

