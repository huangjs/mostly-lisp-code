(defparameter *h* (make-hash-table))

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format nil "Value ~a actually present." value)
        (format nil "Value ~a because key not found." value))))

(setf (gethash 'foo *h*) 'quux) 

(show-value 'foo *h*) 
