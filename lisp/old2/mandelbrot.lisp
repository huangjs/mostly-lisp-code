(declaim (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf :infix))

(defconstant bailout 16d0)
(defconstant max_iterations 1000)

(declaim (ftype (function (double-float double-float) (integer 0 1000)) mandelbrot))
(defun mandelbrot (x y)
  (let ((cr #I "y-0.5")
		(ci x)
		(zi 0d0)
		(zr 0d0))
	(declare (double-float cr ci zi zr))
	(loop for i from 1 to max_iterations
	   do (let ((temp #I "zr*zi")
				(zr2 #I "zr*zr")
				(zi2 #I "zi*zi"))
			(declare (double-float temp zr2 zi2))
			(setf zr #I "zr2-zi2+cr")
			(setf zi #I "temp+temp+ci")
			(when #I "zi2+zr2 > bailout"
			  (return i)))
	   finally (return 0))))

(defun run ()
  (print "Rendering")
  (loop for y from -39d0 to 39d0
	 do (progn
		  (terpri)
		  (loop for x from -39d0 to 39d0
			 do (let ((i (mandelbrot #I "x/40d0" #I "y/40d0")))
				  (if (= i 0)
  					  (princ "*")
  					  (princ " "))))))) 

