(cl:defpackage "CA.PVK.XINTERP"
  (:use #:cl "CA.PVK.TRIVIAL-MATCH"))

(cl:in-package "CA.PVK.XINTERP")

;; Define partial ordering requirements
;; Rewrite everything as :after (remove
;; !exist.)


;;clause: (name :after :before)
(defun rewrite (clauses)
  "Rewrite after and before to only keep befores"
  (let ((live-names (make-hash-table :test 'eql))
	(name-befores (make-hash-table :test 'eql)))
    (dolist (clause clauses)
      (destructuring-bind (name function &key after &allow-other-keys)
	  clause
	(declare (ignore function))
	(setf (gethash name live-names) t)
	(dolist (after-name after)
	  (push name (gethash after-name name-befores)))))
    (mapcar (lambda (clause)
	      (destructuring-bind (name function &key before &allow-other-keys)
		  clause
		(list name function (union (remove-if-not (lambda (name)
							   (gethash name live-names))
							 before)
					  (gethash name
						   name-befores)))))
	    clauses)))

(defun order-clauses (clauses)
  (let ((saved-names (make-hash-table :test 'eql))
	(rewritten-clauses (rewrite clauses)))
    (labels ((inner (to-save saved)
	       (if (null to-save)
		   saved
		   (let ((next (find-if (lambda (clause)
					  (every (lambda (before)
						   (gethash before saved-names))
						 (third clause)))
					to-save)))
		     (if (null next)
			 (error "Error in order-clauses: Circular ordering in ~A~%" to-save)
			 (progn
			   (setf (gethash (first next) saved-names) t)
			   (inner (delete next to-save) (cons (second next) saved))))))))
      (inner (nreverse rewritten-clauses) nil))))

(defparameter *rules* nil
  "List of rules (full definition: name, function, before, after)")

(defparameter *ordered-rules* nil
  "List of ordered functions. Set to nil when *rules* changed.")

(defparameter *remaining-rules* nil
  "Ordered list of functions to try and match with.")

(defvar *current-input* nil
  "Current default input for call-next-rule.")

(defun next-rule-p ()
  (not (null *remaining-rules*))) 

(defun call-next-rule (&optional (*current-input* *current-input*))
  (declare (special *current-input*))
  (unless (next-rule-p)
    (error "No more rule to try."))
  (let ((next-rule (first *remaining-rules*))
	(*remaining-rules* (rest *remaining-rules*)))
    (declare (special *remaining-rules*))
    (funcall next-rule *current-input*)))

(defun replace-or-add (old-value new-value list &key (key #'identity) (test #'eql))
  (let* ((replaced-p nil)
	 (new-list (mapcar (lambda (value)
			     (if (funcall test (funcall key value) old-value)
				 (prog1 new-value
				   (setf replaced-p t))
				 value))
			   list)))
    (if replaced-p
	new-list
	(cons new-value new-list))))

(defun register-rule-handler (name function &key before after)
  (let ((before (if (listp before)
		    before
		    (list before)))
	(after  (if (listp after)
		    after
		    (list after))))
    (setf *rules* (replace-or-add name (list name function :before before :after after)
				  *rules* :key #'first))
    (setf *ordered-rules* nil)))

(defmacro defrule (name (&key before after) pattern &body code)
  (let ((_ (gensym "ARGS")))
    `(register-rule-handler
      ',name (lambda (,_)
	       (match (,_ :fail (call-next-rule) 
			  :block ,name)
		 (,pattern ,@code)))
      :before ',before :after ',after)))

(defun get-rules ()
  (cond ((null *rules*) nil)
	(*ordered-rules* *ordered-rules*)
	(t              (setf *ordered-rules* (order-clauses *rules*)))))

(defmacro with-rule-handler ((name function &key before after) &body body)
  (let ((before (if (listp before)
		    before
		    (list before)))
	(after  (if (listp after)
		    after
		    (list after))))
    `(let ((*rules* (replace-or-add name (list ',name ',function :before ',before :after ',after)
				    *rules* :key #'first))
	   (*ordered-rules* nil))
       (declare (special *rules* *ordered-rules*))
       ,@body)))

(defmacro with-rule ((name (&key before after) pattern &body code) &body body)
  (let ((_ (gensym "ARGS"))
	(before (if (listp before)
		    before
		    (list before)))
	(after  (if (listp after)
		    after
		    (list after))))
    `(let ((*rules* (replace-or-add name (list ',name (lambda (,_)
							(match (,_ :fail (call-next-rule) :block ,name)
							  (,pattern ,@code)))
					       :before ',before
					       :after ',after)
				    *rules* :key #'first))
	   (*ordered-rules* nil))
       (declare (special *rules* *ordered-rules*))
       ,@body)))

(defmacro with-cleared-rules (&body body)
  `(let ((*rules* nil)
	 (*ordered-rules* nil))
     (declare (special *rules* *ordered-rules*))
     ,@body))

(defun interp (expr)
  (let ((*current-input* expr)
	(*remaining-rules* (get-rules)))
    (declare (special *current-input* *remaining-rules*))
    (call-next-rule)))