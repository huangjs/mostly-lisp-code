(defclass unreadable-object ()
  ((text :initarg :text :accessor unreadable-object-text)))

(defmethod print-object ((self unreadable-object) stream)
  "To ensure that unreadable objects are printed as they are read ;-)"
  (print-unreadable-object (self stream :type nil :identity nil)
    (princ (unreadable-object-text self) stream)))

(defun my-#<-reader (stream sub-char arg)
  (declare (ignore sub-char arg))
  (loop :with buffer = (make-array 0 :adjustable t :fill-pointer 0
								   :element-type 'character)
	 :for ch = (read-char stream t nil t)
	 :until (char= #\> ch)
	 :do (vector-push-extend ch buffer)
	 :finally (return (make-instance 'unreadable-object :text buffer))))

(defparameter *my-readtable* (copy-readtable nil))

(SET-DISPATCH-MACRO-CHARACTER #\# #\< (function my-#<-reader) *my-readtable*)

;; (let ((*readtable* *my-readtable*))
;;   (read-from-string "(1 2 #<some unreadable object> 4 5)"))

;; --> (1 2 #<some unreadable object> 4 5) ;
;; 35 
