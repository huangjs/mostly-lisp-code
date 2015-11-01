(defun eval. (e a)
  (cond
	((atom e) (assoc. e a))
	((atom (car e))
	 (cond
	   ((eq (car e) 'quote)		(cadr	e))
	   ((eq (car e) 'atom)		(atom	(eval. (cadr e) a)))
	   ((eq (car e) 'eq)		(eq		(eval. (cadr e) a)
										(eval. (caddr e) a)))
	   ))))
