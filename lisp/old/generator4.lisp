(in-package :cl-user) 

(define-condition yield-signal (condition) 
  ((val :initarg :val :accessor val))) 

(defparameter %current-signal% nil) 

(defun yield (val) 
  (setf (val %current-signal%) val) 
  (restart-case (signal %current-signal%) 
	(next () nil))) 

(defmacro for-each (data in generator &body body) 
  (unless (eq in 'in) 
	(error "for-each syntax error : for-each data _in_ generator")) 
  `(let ((%current-signal% (make-condition 'yield-signal))) 
	 (handler-bind ((yield-signal 
					 (lambda (condition) 
					   (let ((,data (val condition))) 
						 ,@body 
						 (invoke-restart 'next))))) 
	   (funcall ,generator)))) 

(defun make-loop (iterations) 
  (lambda () 
	(loop for i from 1 to iterations 
	   do (yield i)))) 

(defun consumer () 
  (for-each i in (make-loop 1000000) 
	(format nil "~a" nil)))	;nothing here just want to see how fast that is 

#| 
;this here works pretty good, it is possible to have nested generators 
(defun my-loop () 
  (loop for i from 1 to 4 
	 do (yield i))) 

(defun consumer () 
  (for-each i in #'my-loop 
	(for-each j in #'my-loop 
	  (format t "~%i: ~a j: ~a" i j)))) 
|# 
