;;; cmucl only

(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b)))

(defmethod bar ((x foo))
  (declare (ext:slots (slot-boundp foo)))
  (list (slot-value x 'a) (slot-value x 'b)))

(defmethod bar ((x foo))
  (declare (ext:slots (inline (foo a))))
  (list (slot-value x 'a) (slot-value x 'b)))

(defmethod bar ((x foo))
  (list (slot-value x 'a) (slot-value x 'b)))

