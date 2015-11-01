(defmacro alambda (args &body body)
  (labels ((self ,args ,@body))
	#'self))

;;; e.g. (alambda (n) (if (zerop n) 1 (* n (self (1- n)))))


