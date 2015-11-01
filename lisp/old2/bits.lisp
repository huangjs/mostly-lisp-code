(defun integer-to-2-complement-as-bit-vector (n width)
  (labels ((flip-bit (bit)
			 (declare (bit bit))
			 (case bit
			   ((0) 1)
			   ((1) 0)))
		   (flip-bits (bits)
			 (declare (bit-vector bits))
			 (map 'bit-vector
				  #'flip-bit
				  bits)))
	(let* ((result (make-array (list width)
							   :element-type 'bit
							   :initial-element 0))
		   (negativep (if (< n 0) t nil))
		   (original-n n)
		   (n (if negativep (1- (- n)) n)))
	  (loop for i from (1- width) downto 0
		 for complement = (mod n 2)
		 do (progn
			  (setf (aref result i) complement)
			  (setf n (ash n -1)))
		 finally (if (= n 0)
					 (return (if negativep
								 (flip-bits result)
								 result))
					 (error "~a isn't long enough for ~a." width original-n))))))

