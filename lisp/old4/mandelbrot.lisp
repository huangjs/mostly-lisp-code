;;; The Computer Language Shootout
;;;   http://shootout.alioth.debian.org/
;;;
;;; resubmitted by Wade Humeniuk (Fix Stream Problem)
;;; Original contributed by Yannick Gingras <ygingras@ygingras.net>
;;;
;;; To compile
;;; sbcl --load mandelbrot.lisp --eval "(save-lisp-and-die \"mandelbrot.core\" :purify t :toplevel (lambda () (main) (quit)))"
;;; To run
;;; sbcl --noinform --core mandelbrot.core %A
(declaim (optimize (speed 3) (safety 0) (debug 1) (compilation-speed 0)))

(defun render (size stream)
  (declare (type fixnum size) (stream stream))
  
  (assert (and (<= 8 size 10000) (zerop (mod size 8))))
  (let* ((code 0) (bit 0)
         (zr 0.0d0) (zi 0.0d0) (tr 0.0d0)
	 (delta (/ 2d0 size))
         (base-real -1.5d0) (base-imag -1.0d0)
         (buffer (make-array (* size (ceiling size 8)) :element-type '(unsigned-byte 8)))
         (index 0))
    (declare (type (unsigned-byte 8) code)
             (type double-float zr zi tr base-real base-imag delta)
             (type fixnum index bit))

    (dotimes (y size)
      (setf base-imag (- 1.0d0 (* delta y)))
      (dotimes (x size)
	(declare (fixnum x))
	(setf base-real (+ -1.5d0 (* delta x))
	      zr base-real
	      zi base-imag)
        (setf code (logior (ash code 1)
			   (if (dotimes (n 50)
				 (when (< 4.0d0 (+ (* zr zr) (* zi zi)))
				   (return t))
				 (setf tr (+ (* zr zr) (- (* zi zi)) base-real)
				       zi (+ (* 2.0d0 zr zi) base-imag)
				       zr tr))
			       #x00
			       #x01)))
	(when (= (incf bit) 8)
	  (setf (aref buffer index) code
		bit 0 code 0)
	  (incf index))))
    
    (write-sequence buffer stream)))

(defun main (n)
  (declare (optimize (speed 0) (safety 3)))
  (with-open-file (stream "/dev/null" :direction :output :if-exists :supersede :element-type :default)
    (format stream "P4~%~d ~d~%" n n)
    (render n stream)
    (force-output stream)))
