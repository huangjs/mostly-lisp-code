(defclass lock ()
  ((name :initarg :name :reader lock-name))
  (:documentation "The foundation of all locks."))

(defclass null-lock (lock)
  ()
  (:documentation "A lock that is always free."))

(defclass simple-lock (lock)
  ((owner :initform nil :accessor lock-owner))
  (:documentation "A lock is either free or busy."))


;;; constructor functions are recommended
(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))


;;; interface
(defgeneric seize (lock)
  (:documentation
   "Seizes the lock.
Return the lock when the operation succeeds.
Some locks simply wait until they can succeed, while
other locks return NIL if they fail."))

(defgeneric release (lock &optional failure-mode)
  (:documentation
   "Release the lock if it is currently owned by this process.
Return T if the operation succeeds.
If unsuccessful and failure-mode is :no-error, return NIL.
If unsuccessful and failure-mode is :errror, signals an error.
The default for failure-mode is :no-error."))

;;; implementation of interface
(defmethod seize ((l null-lock))
  l)									;return lock, no waiting

(defmethod release ((l null-lock) &optional failure-mode)
  (declare (ignore failure-mode))
  t)


;;; Now assume that we have the following 3 primitives:
;;; 1) without-process-preemption &body body
;;; 2) process-wait reason function &rest arguments
;;; 3) *current-process*

;;; if value of place is old-value, set it to new-value
;;; return t if the setf worked, nil otherwise
(defmacro setf-if (place old-value new-value)
  `(without-process-preemption			;do atomically
	   (cond ((eql ,place ,old-value)
			  (setf ,place ,new-value)
			  t)
			 (t nil))))



(defmethod check-for-mylock ((l simple-lock) process)
  (when (eql (lock-owner l) process)
	(error "Can't seize ~A because you already own it." l)))

(defmethod seize ((l simple-lock))
  (check-for-mylock l *current-process*)
  (do ()
	  ((setf-if (lock-owner l) nil *current-process*))
	(process-wait "Seizing lock"
				  #'(lambda () (null (lock-owner l)))))
  l)

(defmethod release ((l simple-lock)
					&optional (failure-mode :no-error))
  (or (setf-if (lock-owner l) *current-process* nil)
	  (ecase failure-mode
		(:no-error nil)
		(:error (error "~A is not owned by this process" l)))))

