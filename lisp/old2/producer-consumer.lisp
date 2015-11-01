(defpackage :hjs-generator
  (:use :cl :hjs-essential :hjs-macro)
  (:export :yield
		   :yield-signal
		   :for-each
		   :take
		   :enumerate-all
		   :defgenerator))

(in-package :hjs-generator)

(defcondition yield-signal ()
	(value)
	"This condition is used by producer-consumer model. (stateless generator)
The value is: ~a " value)

(defmacro yield (val)
  `(restart-case
	   (signal 'yield-signal :value ,val)
	 (next () nil)))

;;; example1
;; (defun producer ()
;;   (loop for i from 1 to 1000000
;; 	 do (yield i)))

;; (defparameter *test-list* '(a b c d e f))

;; (defun producer2 ()
;;   (loop for i in *test-list*
;; 	 do (yield i)))

;;; deprecated
;; (defun consumer (producer)
;;   (with-collect (collector)
;; 	(handler-bind ((yield-signal
;; 					#'(lambda (c)
;; 						(collector (value-of c))
;; 						(invoke-restart 'next))))
;; 	  (funcall producer))))

;;; example2
;; (consumer #'producer)
;; =>
;; (1 2 3 4 5 6 7 8 9 10)

(defmacro for-each (var in generator &body body)
  (declare (ignore in))
  `(handler-bind ((yield-signal
				   #'(lambda (c)
					   (let ((,var (value-of c)))
						 ,@body
						 (invoke-restart 'next)))))
	 (funcall ,generator)))

(defun enumerate-all (producer)
  (with-collect (collector)
	(for-each i in producer
	  (collector i))))

(defmacro defgenerator (name arg-list &body body)
  `(defun ,name ,arg-list
	 #'(lambda ()
		 ,@body)))

(defun take (n generator)
  (with-collect (collector)
	(block exit
	  (handler-bind ((yield-signal
					  #'(lambda (c)
						  (let ((var (value-of c)))
							(if (> n 0)
								(progn
								  (collector var)
								  (decf n)
								  (invoke-restart 'next))
								(return-from exit nil))))))
		(funcall generator)))))


;;; example2
;; (defgenerator fibs ()
;;   (loop with a = 0
;; 	 with b = 1
;; 	 do (progn
;; 		  (yield b)
;; 		  (psetf a b
;; 				 b (+ a b)))))

;; (defgenerator pi-series ()
;;   (loop for i = 1d0 then (+ i 2)
;; 	 for j = 1d0 then (* j -1)
;; 	 for sum = (/ j i) then (+ sum (/ j i))
;; 	 do (yield (* 4 sum))))

