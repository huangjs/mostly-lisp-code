;;;; (C) 1998 T. Fischbacher -- License: GNU GPL

;;;; TODO: make addition and scalar multiplication keyword args so
;;;; one may just as well use arrays or other arbitrary data structures
;;;; as positions

;;;; The Runge-Kutta4 method:
;;;;
;;;; f0 = f (t,       x)
;;;; f1 = f (t+0.5dt, x + 0.5 f0*dt)
;;;; f2 = f (t+0.5dt, x + 0.5 f1*dt)
;;;; f3 = f (t+dt,    x +     f2*dt)
;;;;
;;;; x' = x + 1/6 (f0 + 2 f1 + 2 f2 + f3)*dt


;;; make-rk4-iterator takes args: (f)
;;; f is a function of two args: (time position)
;;; yielding a "differential position update vector".
;;; (All positions are vectors!)
;;;
;;; The result is a function with args (g start-pos start-time
;;; end-time nsteps), where g is a function of (stepnum time pos) that's
;;; called in every step of the iteration with the time, step count
;;; and position args.

;;; NOTE: result array will not contain start position!
;;; First entry is value after first step.


(defun make-rk4-iterator (f)
  (declare (function f))
  ;;
  #'(lambda (g startpos start-t end-t nsteps)
      (let* ((pos startpos)
	     (n (length startpos))
	     (dt (/ (- end-t start-t) nsteps))
	     (dt/2 (/ dt 2)))
	;; initial call
	(funcall g 0 start-t pos)
	(do ((j 1 (1+ j)))
	    ((> j nsteps))
	  (let* ((delta-t (* dt j))
		 (t3 (+ delta-t start-t))
		 (t0 (- t3 dt))
		 (t12 (- t3 dt/2))
		 ;; We do some consing down there, but that
		 ;;; shouldn't hurt too much. Does it?
		 (f0 (funcall f t0 pos))
		 (f1 (funcall f t12
			      (map 'array
				   #'(lambda (v0 v1) (+ v0 (* 0.5 dt v1)))
				   pos f0)))
		 (f2 (funcall f t12
			      (map 'array
				   #'(lambda (v0 v1) (+ v0 (* 0.5 dt v1)))
				   pos f1)))
		 (f3 (funcall f t3
			      (map 'array
				   #'(lambda (v0 v1) (+ v0 (* dt v1)))
				   pos f2))))
	    (dotimes (k n)
	      (let ((x (+ (aref pos k)
			  (* dt
			     (/ 1.0d0 6.0d0)
			     (+ (aref f0 k)
				(aref f3 k)
				(* 2.0d0
				   (+ (aref f1 k)
				      (aref f2 k))))))))
		(setf (aref pos k) x)))
	    (funcall g j t3 pos)))
	pos ; return end position
	)))

;;; In order to have a comparison -- note that this one simply calculates
;;; a vector of positions and does not perform sophisticated stuff like
;;; make-rk4-iterator


(defun make-primitive-step (f)
  (declare (function f))
  ;;
  #'(lambda (startpos start-t end-t nsteps)
      (let* ((pos startpos)
	     (n (length startpos))
	     (result (make-array nsteps))
	     (dt (/ (- end-t start-t) nsteps)))
	(dotimes (j nsteps)
	  (let ((f0 (funcall f pos)))
	    (setf (aref result j) (make-array n))
	    (dotimes (k n)
	      (let ((x (+ (aref pos k)
			  (* dt (aref f0 k)))))
		(setf (aref pos k) x
		      (aref (aref result j) k) x)))))
	result)))



(defun rktest-propagator (time pos)
  (map 'array #'(lambda (x) (* #c(0 1) x)) pos))

(defun rktest-step-action (n time pos)
  (format t "[~6D]  t=~A: ~A~%" n time pos))

(defun test-it (pos0 t0 t1 nsteps)
  (let ((iter (make-rk4-iterator #'rktest-propagator)))
    (funcall iter #'rktest-step-action pos0 t0 t1 nsteps)))

