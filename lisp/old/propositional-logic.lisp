(defun equiv (&rest args)
  (or (null args)
	  (loop :for p :in (cdr args) :always (eql (not (car args)) (not p)))))

