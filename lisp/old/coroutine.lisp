;;; use arnesi

(defmacro coro (&body body)
  (with-unique-names (coro)
    `(let (,coro)
       (with-call/cc 
		   (let ((yield (lambda/cc (x)
								   (let/cc k
										   (setq ,coro k)
										   x))))
			 (flet ((yield (x)
					  (funcall/cc yield x)))
			   (declare (ignorable (function yield))
						(ignorable yield))
			   (yield :ok)
			   ,@body)))
	   #'(lambda (x) (funcall ,coro x)))))


;;; examples
(defun/cc traverse (tree func)
  (if (consp tree)
      (loop for x in tree do (traverse x func))
      (funcall/cc func tree)))
(defvar *ittr* (coro (traverse *tree* yield)))
