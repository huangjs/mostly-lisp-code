(defun pairify-hash-table (htable)
  (let ((result nil))
	(maphash #'(lambda (x y) (push (cons x y) result))
			 htable)
	result))

(defun fill-hash-table (htable list)
  (dolist (x list)
	;; First, see if the value is already there
	(multiple-value-bind (count found-p)
		(gethash x htable)
	  (cond (found-p
			 ;; value is there, so increment it
			 (setf count (+ count 1))
			 (setf (gethash x htable) count))
			(t
			 ;; The value was not found, so insert it
			 (setf (gethash x htable) 1))))))

(defun occurences (list)
  ;; First, get a hash-table...
  (let ((htable (make-hash-table)))
	;; Then fill it with the values
	(fill-hash-table htable list)
	;; Then extract the dotted-pairs
	(pairify-hash-table htable)))
