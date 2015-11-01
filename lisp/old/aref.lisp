(defgeneric $ (container key)) 
(defgeneric (setf $) (value container key)) 

;;; Implementation for arrays 

(defmethod $ ((container array) (key integer)) 
  (aref container key)) 

(defmethod (setf $) (value (container array) (key integer)) 
  (setf (aref container key) value)) 

