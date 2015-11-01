;;; LA library

;;; Matrix is defined as a 2-dim array

(defun dimension (mat)
  (array-dimensions mat))

(defun m-dim (mat)
  (car (dimension mat)))

(defun n-dim (mat)
  (cadr (dimension mat)))

(defun ith-row (mat i)
  (let ((result (make-array (n-dim mat))))
	(loop for j from 0 below (n-dim mat) do
		 (setf (aref result j)
			   (aref mat (- i 1) j)))
	result))

(defun jth-col (mat j)
  (let ((result (make-array (m-dim mat))))
	(loop for i from 0 below (m-dim mat) do
		 (setf (aref result i)
			   (aref mat i (- j 1))))
	result))

(defun vec2mat (vector &optional (row-or-col :col))
  (if (> (length (array-dimensions vector)) 1)
	  vector
	  (if (eq row-or-col :col)
		  (let ((result (make-array (list (length vector) 1))))
			(loop for i from 0 below (length vector) do
				 (setf (aref result i 0)
					   (aref vector i)))
			result)
		  (let ((result (make-array (list 1 (length vector)))))
			(loop for i from 0 below (length vector) do
				 (setf (aref result 0 i)
					   (aref vector i)))
			result))))

(defun mat2vec (mat)
  (let ((m (m-dim mat))
		(n (n-dim mat))
		(result nil))
	(cond ((= m 1)
		   (setf result (make-array n))
		   (loop for i from 0 below n do
				(setf (aref result i)
					  (aref mat 0 i)))
		   result)
		  ((= n 1)
		   (setf result (make-array m))
		   (loop for i from 0 below m do
				(setf (aref result i)
					  (aref mat i 0)))
		   result)
		  (t (error "matrix is not either a row vector or a col vector.")))))

(defun m.* (a b &optional (row-or-col-a :row) (row-or-col-b :col))
  (let ((a (vec2mat A row-or-col-a))
		(b (vec2mat B row-or-col-b)))
	(let ((ma (m-dim a))
		  (na (n-dim a))
		  (mb (m-dim b))
		  (nb (n-dim b)))
	  (let ((result (make-array (list ma nb))))
		(if (not (= na mb))
			(error "A and B has different dimensions.")
			(progn
			  (loop for i from 0 below ma do
				   (loop for j from 0 below nb do
						(setf (aref result i j)
							  (reduce #'+ (map 'vector #'* (ith-row a (+ i 1)) (jth-col b (+ 1 j))) :initial-value 0))))
			  result))))))

