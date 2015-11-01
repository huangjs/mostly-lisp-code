(in-package :cl-user)

(defmacro funcall-safely (function &rest args)
  `(funcall ,function
	    ,@(loop for arg in args
		    collect `(ignore-errors ,arg))))
