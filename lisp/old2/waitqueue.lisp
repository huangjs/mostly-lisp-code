(use-package :sb-thread)

(defvar *buffer-queue* (make-waitqueue))
(defvar *buffer-lock* (make-mutex :name "buffer lock"))
(defvar *buffer* (list nil))

(defparameter *result* '())

(defun reader ()
  (with-mutex (*buffer-lock*)
	(let ((count 0))
	  (unwind-protect 
		   (loop 
			  (progn
				(condition-wait *buffer-queue* *buffer-lock*)
				(when *buffer*
				  (let ((head (car *buffer*)))
					(setf *buffer* (cdr *buffer*))
					(incf count)
					(format t "reader ~A woke, read ~A, char count = ~A~%"
							(thread-name *current-thread*) head count)))))
		(push count *result*)))))
     
(defun writer (time)
  (loop
	 (sleep (random time))
	 (with-mutex (*buffer-lock*)
	   (let ((el (intern
				  (string (code-char
						   (+ (char-code #\A) (random 26)))))))
		 (setf *buffer* (cons el *buffer*)))
	   (condition-broadcast *buffer-queue*))))
     
(make-thread #'(lambda () (writer 0.01)) :name "queue writer")
(make-thread #'reader :name "queue reader 1")
(make-thread #'reader :name "queue reader 2")
(make-thread #'reader :name "queue reader 3")

(defun test ()
  (unwind-protect
	   (progn
		 (sleep 50)
		 (print "I'm awake.")) 
	(print "finished.")))
