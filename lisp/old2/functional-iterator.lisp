;;; Functional iterator protocol
;;; by Henry Baker
;;; modified by Jianshi Huang

(declaim (optimize (speed 3) (space 0) (debug 1) (safety 0)))

(defun eof (c)
  (declare (type function c))
  (funcall c nil t nil))

(defun gen-tree (x consumer genrest)
  (declare (type function genrest consumer))
  (labels ((genfringel (xl consumer)
             (if (null xl) (funcall genrest consumer)
				 (gen-tree (car xl) consumer
						   #'(lambda (consumer) (genfringel (cdr xl) consumer))))))
    (if (atom x) (funcall consumer x nil genrest)
		(genfringel x consumer))))

(defun gen-list (x consumer genrest)
  (declare (type function genrest consumer))
  (if (null x) (funcall genrest consumer)
	  (funcall consumer (car x) nil
			   #'(lambda (consumer) (gen-list (the list (cdr x)) consumer genrest)))))

(defun gen-vector (x consumer genrest)
  (declare (type function genrest consumer)
		   (type vector x))
  (labels ((myloop (i consumer)
			 (declare (fixnum i)
					  (function consumer))
             (if (= i (length x)) (funcall genrest consumer)
				 (funcall consumer (aref x i) nil
						  #'(lambda (consumer) (myloop (1+ i) consumer))))))
    (myloop 0 consumer)))

(defun make-generator (x gen-fn)
  (lambda (c)
	(funcall gen-fn x c #'eof)))

;;; example
(defun fevery (fn xg yg)	  ; true only if all fn applications are true.
  (declare (type function fn))
  (labels
	  ((myloop (xg yg)
		 (declare (type function xg yg))
		 (funcall xg
				  #'(lambda (x eofx xg)
					  (funcall yg
							   #'(lambda (y eofy yg)
								   (or (and eofx eofy) (and (funcall fn x y) (myloop xg yg)))))))))
	(myloop xg yg)))

(defun deep-every (fn x y)
  (fevery fn (make-generator x 'gen-tree) (make-generator y 'gen-tree)))

(defun samefringe (x y) (deep-every #'eql x y)) ; example of every's use.

(defun fmapcar (fn xg yg) ; accepts 2 generic sequences, returns list of values.
  (declare (type function fn))
  (labels
	  ((myloop (xg yg)
		 (declare (type function xg yg))
		 (funcall xg
				  #'(lambda (x eofx xg)
					  (funcall yg
							   #'(lambda (y eofy yg)
								   (if (or eofx eofy) nil
									   (cons (funcall fn x y) (myloop xg yg)))))))))
	(myloop xg yg)))


;;; closure as generator
;;; FIXME: buggy, x will be modified.
(defun make-tree-generator (x &optional stack)
  (labels ((gen ()
			 (cond ((null x)
					(if (null stack)
						(values nil nil)
						(progn
						  (setf x (pop stack))
						  (gen))))
				   ((not (listp (car x)))
					(let ((y (car x)))
					  (setf x (cdr x))
					  (values y t)))
				   (t
					(push (cdr x) stack)
					(setf x (car x))
					(gen)))))
	#'gen))

(defun gevery (fn xg yg)
  (declare (function fn xg yg))
  (labels ((myloop (xg yg)
			 (multiple-value-bind (x cont-x-p) (funcall xg)
			   (multiple-value-bind (y cont-y-p) (funcall yg)
				 (or (and (not cont-x-p) (not cont-y-p))
					 (and (funcall fn x y) (myloop xg yg)))))))
	(myloop xg yg)))

