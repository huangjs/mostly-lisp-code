(defun divide-simple (numerator denominator)
  (cond ((or (not (numberp numerator))
	     (not (numberp denominator)))
	 (error "(DIVIDE ~s ~s) - Bad arguments."
		numerator denominator))
	((zerop denominator)
	 (error 'division-by-zero
		:operation 'divide
		:operands (list numerator denominator)))
	(t (/ numerator denominator))))

(defun divide-restart (numerator denominator)
  (loop
     (restart-case
	 (divide-simple numerator denominator)
       ;; anonymous restart
       (nil (arg1 arg2)
	 :report "Provide new arguments for use by DIVIDE."
	 :interactive
	 (lambda ()
	   (list (metatilities:prompt-for  'fixnum "Numerator: ")
		 (metatilities:prompt-for  'fixnum "Denominator: ")))
	 (setq numerator arg1 denominator arg2))
       (nil (result)
	 :report "Provide a value to return from DIVIDE."
	 :interactive
	 (lambda () (list (metatilities:prompt-for 'fixnum "Result: ")))
	 (return result)))))

