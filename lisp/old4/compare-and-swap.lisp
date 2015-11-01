(defmacro concurrent-update (place var &body body)
  (let ((prev (gensym)) (func (gensym)))
    `(let ((,prev ,place)
           (,func (lambda (,var) ,@body)))
       (loop :until (eq (sb-ext:compare-and-swap ,place ,prev (funcall ,func ,prev)) ,prev)
	  :do (setf ,prev ,place))
       ,prev)))

(defun make-cstack (&rest elements)
  (cons :stack elements))

(defun push-cstack (element stack)
  (concurrent-update (cdr stack) elts
		     (cons element elts))
  (values))

(defun pop-cstack (stack)
  (car (concurrent-update (cdr stack) elts
			  (cdr elts))))
