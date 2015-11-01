(defun transform-generic (x y a b c d tx ty) 
  ;; 25.42 23.19 21.44 musec. 
  (values (+ tx (* a x) (* b y)) 
		  (+ ty (* c x) (* d y)))) 

(defun transform-float (x y a b c d tx ty) 
  ;; 5.12 4.61 6.45 musec. 
  (declare (type single-float x y a b c d tx ty) 
		   (ftype (function (&rest double-float) double-float) + - * /)) 
  (values (+ tx (* a x) (* b y)) 
		  (+ ty (* c x) (* d y)))) 

(defun transform-float-to-fixnum (x y a b c d tx ty) 
  ;; 2.24 2.53 1.84 musec. 
  (declare (single-float x y a b c d tx ty) 
		   (ftype (function (&rest double-float) double-float) + - * /)) 
  (values (values (the fixnum (truncate (+ tx (* a x) (* b y))))) 
		  (values (the fixnum (truncate (+ ty (* c x) (* d y)))))))


(defun mercator (y a b)					; 57.90 9.56 10.08 musec. 
  ;; Given latitude in degrees >= 0.0, returns the y coordiante of the 
  ;; mercator projection.  This version ignores the eccentricity 
  ;; of the earth, but the error is less than .006. 
  (declare (ftype (function (&rest single-float) single-float) + - * /) 
		   (ftype (function (single-float) single-float) log abs tan) 
		   (single-float y a b)) 
  (values 
   (the fixnum
	 (truncate
	  (+ b
		 (* a
			(the (single-float 0.0 *)
			  (log
			   (the (single-float 1.0s0 *)
				 (abs
				  (tan
				   (+  (float (/ pi 4.0s0) 1.0s0)
					   (* (abs y) 0.008726646259971648s0)))))))))))))
