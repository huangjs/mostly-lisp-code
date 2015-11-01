(defclass point ()
  ((x :initform 0 :initarg :x :accessor point-x)
   (y :initform 0 :initarg :y :accessor point-y)))

(defmethod print-object ((object point) stream)
  (let ((x (point-x object))
		(y (point-y object)))
	(if *print-escape*
		(print-unreadable-object
			(object stream :identity t :type t)
		  (format stream "(~S,~S)" x y))
		(format stream "~S (~S,~S)" (type-of object) x y))))

;;; test
(defvar output-functions
  '(#'print #'princ #'prin1 #'write))
