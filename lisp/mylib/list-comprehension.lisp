;; (defpackage :list-comprehension
;;   (:use "COMMON-LISP")
;;   (:export
;;    "OPEN-BRACKET"
;;    "CLOSING-BRACKET")
;;   (:documentation
;;    "Add two read macros supporting list comprehension"))

;; (in-package :list-comprehension)

(defun open-bracket (stream ch)
  (declare (ignorable stream ch))
  (defmacro comp ((e &rest qs) l2) 
	(if (null qs) `(cons ,e ,l2)		; rule A 
		(let ((q1 (car qs)) 
			  (q (cdr qs))) 
		  (if (not(eq (cadr q1) '<-))		   ; a generator? 
			  `(if ,q1 (comp (,e ,@q),l2) ,l2) ; rule B 
			  (let ((v (car q1))			   ; rule C 
					(l1 (third q1)) 
					(h (gentemp "H-")) 
					(us (gentemp "US-")) 
					(us1 (gentemp "US1-"))) 
				`(labels ((,h (,us)		; corresponds to a letrec 
							(if (null ,us) ,l2 
								(let ((,v (car ,us)) 
									  (,us1 (cdr ,us))) 
								  (comp (,e ,@q) (,h ,us1)))))) 
				   (,h ,l1))))))) 
  (do ((l nil) 
	   (c (read stream t nil t)(read stream t nil t))) 
	  ((eq c '|]|) `(comp ,(reverse l) ())) 
	(push c l))) 


(defun closing-bracket (stream ch)
  (declare (ignorable stream ch))
  `|]|)

;; (in-package :common-lisp)

(eval-when (:compile-toplevel :load-toplevel)
  (set-macro-character #\[ #'open-bracket)
  (set-macro-character #\] #'closing-bracket))


;;; examples

;;; outer-product
;; (defun outer-product (xs ys)
;;   "also called as tensor product in mathematics.
;; e.g. (outer-product '(1 2) '(4 5 6))
;; 		=>
;; 		((4 5 6) (8 10 12))"
;;   (flet ((scalar-product (a x)
;; 		   (mapcar #'(lambda (e) (* a e))
;; 				   x)))
;; 	[ (scalar-product x ys) (x <- xs) ]))

