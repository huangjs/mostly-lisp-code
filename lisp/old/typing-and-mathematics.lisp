;;; This program is largely about CLOS

;;; first, T is a subclass of itself
(setf universe (find-class 't))
(typep universe universe)

;;; the TP class
(defclass TP ()
  ((name :type symbol :initarg :name :initform (gensym) :reader name))
  (:metaclass aclmop:funcallable-standard-class))

(defmethod print-object ((tp tp) stream)
  (format stream
		  "#<~S ~S>"
		  (class-name (class-of tp)) (name tp)))

(defmethod initialize-instance :after ((tp tp) &rest ignored)
  (declare (ignore ignore))
  (set (name tp) tp))


;;; the DTP class
(defclass DTP ()
  ((sub :type list :accessor sub)
   (sup :type list :accessor sup))
  (:metaclass funcallable-standard-class))

;;; example
(make-instance 'dtp :name 'void)
(setf (sub void) '(void)
	  (sup void) '(void any))
(aclmop:set-funcallable-instance-function void
		#'(lambda (obj)
			(declare (type t obj))
			(the boolean nil)))			;make void funcallable

