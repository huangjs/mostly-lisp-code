(defpackage :linalg
  (:use :cl :iter)
  (:export :matrix
	   :copy-matrix
	   ))

(in-package :linalg)

(defclass matrix ()
  ((rows
    :initarg :rows
    :initform (error ":rows must be specified.")
    :reader matrix-rows)
   (cols
    :initarg :cols
    :initform (error ":cols must be specified.")
    :reader matrix-cols)
   (data
    :initarg :data
    :accessor matrix-data)))

(defmethod initialize-instance :after ((m matrix) &key generator)
  (with-slots (rows cols data) m
    (assert (< 0 rows)
	    nil
	    ":rows must be > 0.")
    (assert (< 0 cols)
	    nil
	    ":cols must be > 0.")
    (if (slot-boundp m 'data)
	(progn
	  (assert (= (length data) (* rows cols))
		  nil
		  ":data dimension should be ~d."
		  (* rows cols))
	  (assert (not generator)
		  nil
		  ":data and :generator may not be specified at the same time."))
	(if (functionp generator)
	    (progn
	      (setf data (make-array (list rows cols)
				     :element-type 'double-float))
	      (let ((data data))
		(declare (type (simple-array double-float (* *))))
		(iter (for (the fixnum i) below rows)
		      (iter (for (the fixnum j) below cols)
			    (setf (aref data i j)
				  (funcall generator i j))))))
	    (progn
	      (setf data
		    (make-array (list rows cols)
				:element-type 'double-float
				:initial-element 0.0d0)))))))


(defun copy-matrix (m)
  (make-instance 'matrix
		 :rows (matrix-rows m)
		 :cols (matrix-cols m)
		 :data (alexandria:copy-array (matrix-data m))))

(defun transposed (m)
  (let ((data (matrix-data m)))
    (declare (type (simple-array double-float (* *))))
    (make-instance 'matrix
		   :rows (matrix-cols m)
		   :cols (matrix-rows m)
		   :generator (lambda (i j) (aref data i j)))))

(defun matrix-print (m)
  (let ((data (matrix-data m)))
    (declare (type (simple-array double-float (* *))))
    (dotimes (i (matrix-rows m))
      (dotimes (j (matrix-cols m))
	(format t "~7,2f  " (aref data i j)))
      (terpri))))

