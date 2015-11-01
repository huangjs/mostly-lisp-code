;;; from slow to fast implementation
(proclaim '(optimize (speed 3) (debug 0) (safety 0) (space 0)))


(defun test1 ()
  (let ((a (make-array 0 :adjustable t :fill-pointer 0))
		(b nil))
	(dotimes (i 1000000)
	  (progn
		(vector-push-extend "What do you know" a)
		(vector-push-extend "so long ..." a)
		(vector-push-extend "chicken crosses road" a)
		(vector-push-extend "fool" a)))
	(setf b (remove-duplicates a :test #'string-equal))
 	(map 'vector #'print b)))

(defun test2 ()
  (let ((a (make-array 0 :adjustable t :fill-pointer 0))
		(b nil))
	(dotimes (i 1000000)
	  (progn
		(vector-push-extend "What do you know" a)
		(vector-push-extend "so long ..." a)
		(vector-push-extend "chicken crosses road" a)
		(vector-push-extend "fool" a)))
	(setf b (remove-duplicates a :test #'eq))
 	(map 'vector #'print b)))

(defun test3 ()
  (let ((a (make-array 4000000 :element-type 'string
					   :adjustable nil
					   :fill-pointer 0))
		(b nil))
	(dotimes (i 1000000)
	  (progn
		(vector-push "What do you know" a)
		(vector-push "so long ..." a)
		(vector-push "chicken crosses road" a)
		(vector-push "fool" a)))
	(setf b (remove-duplicates a))
	(map 'vector #'print b)))

(defun test4 ()
  (let ((a (make-array 4000000 :element-type 'string
					   :adjustable nil))
		(b nil))
	(dotimes (i 1000000)
	  (progn
		(let ((j (1- (* 4 i))))
		  (setf (aref a (incf j)) "What do you know")
		  (setf (aref a (incf j)) "so long ...")
		  (setf (aref a (incf j)) "chicken crosses road")
		  (setf (aref a (incf j)) "fool"))))
	(setf b (remove-duplicates a))
	(map 'vector #'print b)))

(defun test5 ()
  (let ((a (make-array 4000000 :element-type 'list
					   :adjustable nil))
		(b nil))
	(dotimes (i 1000000)
	  (progn
		(let ((j (1- (* 4 i))))
		  (setf (aref a (incf j)) '(What do you know))
		  (setf (aref a (incf j)) '(so long))
		  (setf (aref a (incf j)) '(chicken crosses road))
		  (setf (aref a (incf j)) '(fool)))))
	(setf b (remove-duplicates a))
	(map 'vector #'print b)))

(defun test-list ()
  (let ((a nil)
		(b nil))
	(dotimes (i 1000000)
	  (progn
		(push "What do you know" a)
		(push "so long ..." a)
		(push "chicken crosses road" a)
		(push "fool" a)))
	(setf b (remove-duplicates a))
	(map 'list #'print b)))

