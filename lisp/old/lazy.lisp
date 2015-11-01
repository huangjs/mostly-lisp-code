(defpackage lazy
  (:use :cl)
  (:shadow cl:cdr cl:cons cl:mapcar cl:list-length cl:dolist)
  (:export   "CDR"  "CONS"  "MAPCAR"  "LIST-LENGTH"  "DOLIST"))

(in-package :lazy)

;;; constructors
(defstruct (delay
             (:constructor make-delay (closure))
             (:predicate delayp))
  closure)

(defmethod print-object ((self delay) stream)
  (princ "..." stream))

(defmacro delay (form)
  `(make-delay #'(lambda () ,form)))

(defun force (thing)
  (if (delayp thing)
	  (funcall (delay-closure thing))
	  thing))


;;; support functions
(defun lazy:cdr (cons)
  (setf (cl:cdr cons) (force (cl:cdr cons))))

(defmacro lazy:cons (head tail)
  `(cl:cons ,head (delay ,tail)))

(defun lazy:mapcar (function list &rest more-lists)
  (cons (apply function (car list) (cl:mapcar #'car more-lists))
        (apply #'lazy:mapcar function (cdr list) (cl:mapcar #'cdr more-lists))))

(defun lazy:list-length (list)
  (labels ((ll (list n)
             (cond ((consp list) (ll (cl:cdr list) (1+ n)))
                   ((null list) n)
                   (t nil))))
    (ll list 0)))

(defmacro lazy:dolist ((var list &optional value) &body body)
  (let ((s (gentemp)))
    `(do* ((,s ,list (cdr ,s))
           (,var (car ,s) (car ,s)))
          ((null ,s) ,value)
       ,@body)))


;;; helper function
(defun first-n (list n)
  (and list
       (plusp n)
       (cl:cons (car list) (first-n (cdr list) (1- n)))))

(defun numbers (&optional (start 1) (end nil))
  (unless (and end (> start end))
    (cons start (numbers (1+ start) end))))


;;; examples
(defun primes ()
  (labels ((filter-multiples-of (n list)
             (if (zerop (mod (car list) n))
				 (filter-multiples-of n (cdr list))
				 (cons (car list) (filter-multiples-of n (cdr list)))))
           (primes-from (n)
             (cons n (filter-multiples-of n (primes-from (1+ n))))))
    (cons 1 (primes-from 2))))
