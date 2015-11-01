(defclass shape ()
  ()
  (:documentation "The foundation of all shapes."))

(defclass triangle (shape)
  ((a :reader side-a :initarg :side-a)
   (b :reader side-b :initarg :side-b)
   (c :reader side-c :initarg :side-c)))

(defun make-triangle (a b c)
  ;; All sides should be represented as floats
  (make-instance 'triangle :side-a (coerce a 'float)
				 :side-b (coerce b 'float)
				 :side-c (coerce c 'float)))

;;; Return the angle A between adjacent sides b and c
;;; and opposite side a, given all sides of a triangle
;;; Law of Cosines: a^2 = b^2 + c^2 -2bc(cos A)
(defun three-sides-to-angle (a b c)
  (acos (/ (- (+ (expt b 2)
				 (expt c 2))
			  (expt a 2))
		   (* 2 b c))))

(defmethod angle-A ((tri triangle))
  (three-sides-to-angle (side-a tri)
						(side-b tri)
						(side-c tri)))

(defmethod angle-B ((tri triangle))
  (three-sides-to-angle (side-b tri)
						(side-c tri)
						(side-a tri)))

(defmethod angle-C ((tri triangle))
  (three-sides-to-angle (side-c tri)
						(side-a tri)
						(side-b tri)))

(defgeneric dimensions (shape)
  (:documentation "Return list of side lengths."))

(defgeneric angles (shape)
  (:documentation "Returns list of angles."))

(defgeneric area (shape)
  (:documentation "Returns area of the shape."))


(defmethod dimensions ((tri triangle))
  (list (side-a tri)
		(side-b tri)
		(side-c tri)))

(defmethod angles ((tri triangle))
  (list (angle-A tri)
		(angle-B tri)
		(angle-C tri)))

;;; Return the area of a triangle
;;; Algorithm is: area = ab(sin C)/2
(defmethod area ((tri triangle))
  (* (side-a tri) (side-b tri)
	 (sin (angle-C tri))
	 .5))


;;; Note: this needed to be evaluated first!
;;; Otherwise all the side-c slot will be lost!
;;; safe method
(defmethod update-instance-for-redefined-class :after
	((instance triangle)
	 added-slots discarded-slots
	 plist &rest initargs)
  (declare (ignore initargs))
  ;; Identify this particular redefinition
  (if (and (member 'c discarded-slots)
		   (member 'angle-C added-slots))
	  (setf (slot-value instance 'angle-C)
			(three-sides-to-angle
			 (getf plist 'c)
			 (side-a instance)
			 (side-b instance)))))

;;; Changing the representation of triangles
(defclass triangle (shape)
  ((a :reader side-a :initarg :side-a)
   (b :reader side-b :initarg :side-b)
   (angle-C :reader angle-C :initarg :angle-C)))


(defmethod side-c ((tri triangle))
  (third-side (side-a tri)
			  (side-b tri)
			  (angle-C tri)))

;;; Algorithm is: c^2 = a^2 + b^2 - 2ab(cos c)
(defun third-side (a b angle-C)
  (sqrt (- (+ (expt a 2)
			  (expt b 2))
		   (* 2 a b (cos angle-C)))))

(defun make-triangle (a b c)
  (let* ((float-a (coerce a 'float))
		 (float-b (coerce b 'float))
		 (float-c (coerce c 'float))
		 (angle-C (three-sides-to-angle
				   float-a float-b float-c)))
	(make-instance 'triangle :side-a float-a
				   :side-b float-b
				   :angle-C angle-C)))

