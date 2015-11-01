(defvar *thread-count* 0)
(defvar *thread-queue* (sb-thread:make-waitqueue))
(defvar *thread-lock* (sb-thread:make-mutex))

(defun thread-done ()
  (sb-thread:with-mutex (*thread-lock*)
	(decf *thread-count*)
	(sb-thread:condition-notify *thread-queue*)))

(defun wait-for-threads ()
  (loop
	 (sb-thread:with-mutex (*thread-lock*)
	   (when (zerop *thread-count*)
		 (return))
	   (sb-thread:condition-wait *thread-queue* *thread-lock*))))

(defun foo ()
  (loop repeat (expt 2 27) sum 1)
  (thread-done))

(defun make-threads (n)
  (dotimes (i n)
	(incf *thread-count*)
	(sb-thread:make-thread #'foo)))

(print 'one-thread)

(time
 (progn
   (make-threads 1)
   (wait-for-threads)))

(print 'two-threads)

(time
 (progn
   (make-threads 2)
   (wait-for-threads)))

(print 'three-threads)

(time
 (progn
   (make-threads 3)
   (wait-for-threads)))

(print 'done)
