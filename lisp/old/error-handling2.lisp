(defun slope (x1 y1 x2 y2)
  (/ (- y2 y1) (- x2 x1)))

(defun print-slope (x1 y1 x2 y2)
  (handler-case
	  (print (slope x1 y2 x2 y2))
	(division-by-zero ()
	  (print :infinite))
	(error (c)
	  (princ c))))

;;; user defined condition
(define-condition network-down (error)
  ((network-type :initarg :type))
  (:report
   (lambda (condition stream)
	 (format stream
			 "The ~A network is down"
			 (slot-value condition 'network-type)))))

(defun check-network ()
  (or (test-local-area-network)
	  (signal 'network-down :type :LAN)))

(defun test-local-area-network ()
  nil)


;;; restarts
(defun abort-if-abortable (value)
  (when (find-restart 'abort)
	(invoke-restart 'abort value)))

