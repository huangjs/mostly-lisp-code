(declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype uint () '(unsigned-byte 32))
(deftype gint () '(integer
		   #.(+ 2 most-negative-fixnum)
		   #.(- most-positive-fixnum 2)))
(deftype guint () '(integer
		   0
		   #.(- most-positive-fixnum 2)))

;;; (ack 3 n)
(defun ack (m n)
  (declare (gint m n))
  (if (zerop m)
      (1+ n)
      (ack (1- m) (if (zerop n)
		      1
		      (ack m (1- n))))))

(defun fibo (n)
  (declare (fixnum n))
  (if (< n 2)
      1
      (the fixnum (+ (the fixnum (fibo (1- n))) (the fixnum (fibo (- n 2)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +size+ 512))

(defun matrix (n)
  (declare (fixnum n))
  (let ((m1 (make-array '(#.+size+ #.+size+) :element-type 'uint))
	(m2 (make-array '(#.+size+ #.+size+) :element-type 'uint))
	(mm (make-array '(#.+size+ #.+size+) :element-type 'uint)))
    (dotimes (i n)
      (declare (ignorable i))
      (dotimes (i #.+size+)
	(dotimes (j #.+size+)
	  (let ((val 0))
	    (declare (fixnum val))
	    (dotimes (k #.+size+)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)

	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)

	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)

	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))
	      (incf k)
	      (incf val (the fixnum (* (aref m1 i k) (aref m2 k j))))

	      )
	    (setf (aref mm i j) val)))))))