;;; mine
(defun pack-tbl-as-byte8 (tbl)
  (flet ((pack! (x v)
		   (declare (type (unsigned-byte 32) x))
		   (cond ((<= x #xFF)
				  (vector-push-extend (ldb (byte 8 0) x) v))
				 ((<= x #xFFFF)
				  (vector-push-extend (ldb (byte 8 8) x) v)
				  (vector-push-extend (ldb (byte 8 0) x) v))
				 (t
				  (vector-push-extend (ldb (byte 8 24) x) v)
				  (vector-push-extend (ldb (byte 8 16) x) v)
				  (vector-push-extend (ldb (byte 8 8) x) v)
				  (vector-push-extend (ldb (byte 8 0) x) v)))))
	(declare (inline pack!))
	(loop with num = (first (array-dimensions tbl))
	   with size = (* 8 num)
	   with result = (make-array size
								 :element-type '(unsigned-byte 8)
								 :fill-pointer 0
								 :adjustable t)
	   for i of-type fixnum from 0 below num
	   do (progn
			(pack! (aref tbl i 0) result)
			(pack! (aref tbl i 1) result))
	   finally (return result))))

(defun pack-tbl-as-byte16 (tbl)
  (flet ((pack! (x v)
		   (declare (type (unsigned-byte 32) x))
		   (cond ((<= x #xFFFF) 
				  (vector-push-extend (ldb (byte 16 0) x) v))
				 (t
				  (vector-push-extend (ldb (byte 16 16) x) v)
				  (vector-push-extend (ldb (byte 16 0) x) v)))))
	(loop with num = (first (array-dimensions tbl))
	   with size = (* 4 num)
	   with result = (make-array size
								 :element-type '(unsigned-byte 16)
								 :fill-pointer 0
								 :adjustable t)
	   for i of-type fixnum from 0 below num
	   do (progn
			(pack! (aref tbl i 0) result)
			(pack! (aref tbl i 1) result))
	   finally (return result))))

(defun pack-tbl-as-byte32 (tbl)
  (loop with num = (first (array-dimensions tbl))
	 with size = (* 2 num)
	 with result = (make-array size
							   :element-type '(unsigned-byte 32)
							   :fill-pointer 0
							   :adjustable t)
	 for i of-type fixnum from 0 below num
	 do (progn
		  (vector-push-extend (aref tbl i 0) result)
		  (vector-push-extend (aref tbl i 1) result))
	 finally (return result)))
