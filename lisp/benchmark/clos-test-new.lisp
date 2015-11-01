(proclaim '(optimize (safety 0) (speed 3) (debug 0)))

(defstruct (thing-st (:conc-name th-)
		     (:type vector))
  name
  (value 0.0))

(defclass thing-cl ()
  ((name)
   (value :type single-float :initform 0.0 :accessor value)))

(defun fun-inc-value-class (thing-cl val)
  (declare (type (simple-vector 2) thing-cl))
  (declare (single-float val))
  (let ((v (slot-value thing-cl 'value)))
    (setf v (the single-float (+ v val)))))

(defmethod met-inc-value-class (thing-cl val)
  (declare (type (simple-vector 2) thing-cl))
  (declare (single-float val))
  (let ((v (slot-value thing-cl 'value)))
    (setf v (the single-float (+ v val)))))


(defun fun-inc-value-class-with (thing-cl val)
  (declare (type (simple-vector 2) thing-cl))
  (declare (single-float val))
  (with-slots (value) thing-cl
    (setf value (the single-float (+ value val)))))

(defmethod met-inc-value-class-with (thing-cl val)
  (declare (type (simple-vector 2) thing-cl))
  (declare (single-float val))
  (with-slots (value) thing-cl
    (setf value (the single-float (+ value val)))))


(defun fun-inc-value-class-acc (thing-cl val)
  (declare (type (simple-vector 2) thing-cl))
  (declare (single-float val))
  (setf (value thing-cl) (the single-float (+ (value thing-cl) val))))

(defmethod met-inc-value-class-acc (thing-cl val)
  (declare (type (simple-vector 2) thing-cl))
  (declare (single-float val))
  (setf (value thing-cl) (the single-float (+ (value thing-cl) val))))


(defun fun-inc-value-struc (thing val)
  (declare (single-float val))
  (setf (th-value thing) (the single-float (+ val (th-value thing)))))

(defmethod met-inc-value-struc (thing val)
  (declare (single-float val))
  (setf (th-value thing) (the single-float (+ val (th-value thing)))))


(compile 'fun-inc-value-class-with)
(compile 'met-inc-value-class-with)

(compile 'fun-inc-value-class)
(compile 'met-inc-value-class)

(compile 'fun-inc-value-class-acc)
(compile 'met-inc-value-class-acc)

(compile 'fun-inc-value-struc)
(compile 'met-inc-value-struc)


(defun test (iter)
  (let ((obj (make-instance 'thing-cl)))
    
    (print "Function on class (with-slots)")
    (time (dotimes (i iter)
	    (fun-inc-value-class-with obj 15.101)))

    (print "Method on class (with-slots)")
    (time (dotimes (i iter)
	    (met-inc-value-class-with obj 15.101)))
    
    (print "Function on class (slot-value)")
    (time (dotimes (i iter)
	    (fun-inc-value-class obj 15.101)))
    
    (print "Method on class (slot-value)")
    (time (dotimes (i iter)
	    (met-inc-value-class obj 15.101)))
    
    
    (print "Function on class (accessor)")
    (time (dotimes (i iter)
	    (fun-inc-value-class-acc obj 15.101)))
    
    (print "Method on class (accessor)")
    (time (dotimes (i iter)
	    (met-inc-value-class-acc obj 15.101))))


  (let ((obj (make-thing-st)))
       (print "Function on structure")
       (time (dotimes (i iter)
	       (fun-inc-value-struc obj 15.101)))
       
       (print "Method on structure")
       (time (dotimes (i iter)
	       (met-inc-value-struc obj 15.101)))))

(compile 'test)

(test 100000)
