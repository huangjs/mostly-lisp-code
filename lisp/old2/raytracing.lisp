(declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))

(defconstant infinity (coerce most-positive-single-float 'double-float))
(defconstant delta (sqrt (coerce single-float-epsilon 'double-float)))

(declaim (inline vec v* v+ v- unitise ray vec make-ray make-sphere x y z
				 ray_trace))

(defstruct (vec (:conc-name nil) (:constructor vec (x y z)))
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (z 0d0 :type double-float))

(defun v* (s r)
  (declare (type double-float s)
		   (type vec r))
  (the vec (vec (* s (x r)) (* s (y r)) (* s (z r)))))

(defmacro defvfun (name op)
  `(defun ,name (a b)
	 (declare (type vec a b))
	 (let ((xa (x a)) (ya (y a)) (za (z a))
		   (xb (x b)) (yb (y b)) (zb (z b)))
	   (vec (,op xa xb)
			(,op ya yb)
			(,op za zb)))))

(defvfun v+ +)
(defvfun v- -)

(defmacro dot (a b)
  `(+ (* (x ,a) (x ,b)) (* (y ,a) (y ,b)) (* (z ,a) (z ,b))))

(defun unitise (r)
  (declare (type vec r))
  (v* (/ 1d0 (the double-float (sqrt (the double-float (dot r r))))) r))

(defstruct (ray (:conc-name nil))
  (orig nil :type vec)
  (dir nil :type vec))

(defun ray (orig dir) (make-ray :orig orig :dir dir))

(defstruct (sphere (:conc-name nil))
  (center (vec 0d0 0d0 0d0) :type vec)
  (radius 0d0 :type double-float))

;;(shadow 'group)
(defstruct (group (:conc-name nil) (:include sphere))
  (children () :type list))

(defun ray_sphere (ray sphere)
  (let* ((v    (v- (center sphere) (orig ray)))
         (b    (dot v (dir ray)))
         (disc (+ (- (* b b) (dot v v)) (* (radius sphere) (radius
															sphere)))))
    (if (< disc 0d0) infinity
        (let ((disc (sqrt disc)))
          (let ((t2 (+ b disc))
                (t1 (- b disc)))
            (cond ((< t2 0d0) infinity)
                  ((> t1 0d0) t1)
                  (t t2)))))))

(defun intersect (ray scene)
  (labels ((aux (hit scene)
             (destructuring-bind (lam . _) hit
               (declare (ignore _) (double-float lam))
			   (etypecase scene
				 (group
				  (if (>= (the double-float (ray_sphere ray scene)) lam)
					  hit
					  (reduce #'aux (children scene) :initial-value hit)))
				 (sphere
				  (let ((lamt (the double-float (ray_sphere ray scene))))
					(if (>= lamt lam) hit
						(cons lamt (unitise (v- (v+ (orig ray) (v* lamt (dir
																				 ray))) (center scene)))))))))))
    (aux `(,infinity . (vec 0d0 0d0 0d0)) scene)))

(defun ray_trace (light ray scene)
  (destructuring-bind (lam . normal) (intersect ray scene)
    (declare (double-float lam))
    (if (= lam infinity) 0d0
		(let ((g (dot normal light)))
		  (if (>= g 0d0) 0d0
			  (let ((p (v+ (v+ (orig ray) (v* lam (dir ray))) (v* delta
																  normal))))
				(destructuring-bind (lam . _)
					(intersect (ray p (v* -1d0 light)) scene)
				  (declare (ignore _) (double-float lam))
				  (if (< lam infinity) 0d0 (- g)))))))))

(defun create (n c r)
  (declare (double-float r)
           (fixnum n))
  (let ((obj (make-sphere :center c :radius r)))
    (if (= n 1)
        obj
        (let ((rt (* 3d0 (/ r (the double-float (sqrt 12d0))))))
          (labels ((aux (x z) (create (1- n) (v+ c (vec x rt z)) (/ r
																	2d0))))
            (make-group :center c
                        :radius (* 3d0 r)
                        :children (list* obj (mapcar #'aux
                                                     (list (- rt) rt (- rt)
														   rt)
                                                     (list (- rt) (- rt) rt
														   rt)))))))))

(defun main (level file-name n ss)
  (declare (fixnum level n ss))
  (let ((scene (create level (vec 0d0 -1d0 0d0) 1d0))
        (light (unitise (vec -1d0 -3d0 2d0)))
        (-n/2 (- (/ (coerce n 'double-float) 2d0)))
        (1-n/2 (1- (/ (coerce n 'double-float) 2d0)))
		(nn (coerce n 'double-float))
		(sss (coerce ss 'double-float)))
    (with-open-file (s
					 file-name :if-exists :supersede :if-does-not-exist :create :direction :output)
      (format (the stream s) "P5~%~A ~A~%255~%" n n))
    (with-open-file (s file-name :element-type '(unsigned-byte
												 8) :if-exists :append :direction :output)
      (loop for y of-type double-float from 1-n/2 downto -n/2
		 ;;do (sb-ext:gc-off)
		 ;;do (print y)
		 do (loop for x of-type double-float from -n/2 to 1-n/2
			   do (let ((g 0d0))
					(declare (double-float g))
					(loop for dx of-type double-float from x below (1+ x) by (/
																			  1d0 sss)
                       do (loop for dy of-type double-float from y below (1+
																		  y) by (/ 1d0 sss) 
							 do (let ((d (unitise (vec dx dy nn))))
								  (incf g (ray_trace light (ray (vec 0d0
																	 0d0 -4d0) d) scene)))))
					;; (let ((g (+ 0.5 (* 255d0 (/ g (* sss sss))))))
;; 					  (write-byte (floor g) s))
					)))))) 
