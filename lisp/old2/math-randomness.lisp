(defconstant +other-label+ 'others)

(defun make-vector-iterator (vector)
  (let ((length (length vector))
		(index 0))
	#'(lambda ()
		(if (< index length)
			(let ((result (aref vector index)))
			  (incf index)
			  (values result t))
			(values nil nil)))))

(defun make-list-iterator (list)
  (let ((length (length list)))
	#'(lambda ()
		(if (> length 0)
			(let ((result (first list)))
			  (decf length)
			  (setf list (rest list))
			  (values result t))
			(values nil nil)))))

(defun make-accumulator (list-of-labels &key (test #'eql))
  (let ((accumulator (make-hash-table :test test :size (length list-of-labels))))
	(dolist (e list-of-labels)
	  (setf (gethash e accumulator) 0))
	(setf (gethash +other-label+ accumulator) 0)
	accumulator))

(defmacro do-accumulator ((label quantity accumulator &optional result) &body body)
  (once-only (accumulator result)
	`(loop for ,label being the hash-keys in ,accumulator using (hash-value ,quantity)
		do (progn ,@body)
		finally (when ,result (return ,result)))))

(defun map-accumulator (function accumulator)
  (let ((result nil))
	(do-accumulator (l q accumulator)
	  (push (funcall function (list l q accumulator)) result))
	(nreverse result)))

(defun find-slot (input accumulator)
  (multiple-value-bind (quantity has-val-p) (gethash input accumulator)
	(when has-val-p
	  (list input quantity accumulator))))

(defun slot-label (slot)
  (first slot))

(defun slot-quantity (slot)
  (second slot))

(defun (setf slot-quantity) (value slot)
  (let ((label (slot-label slot))
		(accumulator (third slot)))
	(setf (gethash label accumulator) value)))

(defun sample-one (input accumulator)
  (let ((slot (find-slot input accumulator)))
	(if slot
		(incf (slot-quantity slot))
		(incf (slot-quantity (find-slot +other-label+ accumulator))))
	accumulator))

(defun sample-all (inputs accumulator &key (make-iterator-fn #'make-list-iterator))
  (let ((iterator (funcall make-iterator-fn inputs)))
	(loop
	   (multiple-value-bind (input has-val-p)
		   (funcall iterator)
		 (if (null has-val-p)
			 (return accumulator)
			 (sample-one input accumulator))))))

(defun print-accumulator (accumulator)
  (let ((sum (apply #'+ (map-accumulator #'slot-quantity accumulator))))
	(do-accumulator (l q accumulator)
	  (let ((num-of-stars (round (* 50 (/ q sum)))))
		(format t "~&~A: ~A ~,2f% ~{~A~}~%"
				l q
				(coerce (* 100 (/ q sum)) 'float)
				(make-list num-of-stars :initial-element #\*))))
	accumulator))


(defun count-number-appearance-literally (start end &key (base 10))
  (let* ((format-string (concatenate 'string "~" (write-to-string base) "r"))
		 (num-set (loop for i below base
					 collect (aref (format nil format-string i) 0)))
		 (accumulator (make-accumulator num-set)))
	(loop for i from start to end
	   for literal-num = (format nil format-string i)
	   do (sample-all literal-num accumulator
					  :make-iterator-fn #'make-vector-iterator)
	   finally (print-accumulator accumulator))))

