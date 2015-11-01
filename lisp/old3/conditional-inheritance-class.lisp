#+sbcl(use-package :sb-mop)


(defclass conditional-inheritance-class (standard-class) ())

(defclass my-direct-slot-definition (standard-direct-slot-definition)
  ((inherit? :accessor inherit? :initarg :inherit? :initform t)))

(defclass my-effective-slot-definition (standard-effective-slot-definition)
  ((inherit? :accessor inherit? :initarg :inherit? :initform t)))

(defmethod direct-slot-definition-class ((class conditional-inheritance-class) &key &allow-other-keys)
  (find-class 'my-direct-slot-definition))

(defmethod effective-slot-definition-class ((class conditional-inheritance-class) &key &allow-other-keys)
  (find-class 'my-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class conditional-inheritance-class) slot-name direct-slot-definitions)
  (declare (ignorable slot-name))
  (let ((effective-slot-def (call-next-method)))
    (setf (inherit? effective-slot-def)
          (inherit? (first direct-slot-definitions)))
    effective-slot-def))

(defmethod validate-superclass ((class conditional-inheritance-class) super)
  t)

(defmethod compute-slots ((class conditional-inheritance-class))
  (loop for slot in (call-next-method)
	 when (or (inherit? slot)
			  (member (slot-definition-name slot) (class-direct-slots class)
					  :key #'slot-definition-name))
	 collect slot))


;;; usage
#|
 (defclass foo ()
  (a1 a2 a3 a4 a5
	  (a6 :inherit? nil))
  (:metaclass conditional-inheritance-class))

;; Instances will inherit only slots a1-a5. Not a6.
 (defclass bar (foo)
  ()
  (:metaclass conditional-inheritance-class)) 
|#

