#|
Macro:

catch-continuation tag throw-cont non-throw-cont body...
catch-continuation-if cond-form tag throw-cont non-throw-cont body...

The catch-continuation special form makes it convenient to
discriminate whether exit is normal or due to a throw. The body is
executed inside a catch on tag (which is evaluated). If body returns
normally, the function non-throw-cont is called, passing all the
values returned by the last form in body as arguments. This function's
values are returned from the catch-continuation. If on the other hand
a throw to tag occurs, the values thrown are passed to the function
throw-cont, and its values are returned. If a continuation is
explicitly written as nil, it is not called at all. The arguments that
would have been passed to it are returned instead. This is equivalent
to using values as the function; but explicit nil is optimized, so use
that. catch-continuation-if differs only in that the catch is not done
if the value of the cond-form is nil. In this case, the non-throw
continuation if any is always called. In the general case, consing is
necessary to record the multiple values, but if a continuation is an
explicit #'(lambda ...) with a fixed number of arguments, or if a
continuation is nil, it is open coded and the consing is avoided.

|#


(in-package :cl-user)

(defmacro catch-continuation (tag throw-cont non-throw-cont &body body)
  (alexandria:once-only (throw-cont non-throw-cont)
    (alexandria:with-unique-names (exit caught)
      `(let ((,caught nil))
	 (multiple-value-call
	     (lambda (&rest args)
	       (if ,caught
		   (if ,throw-cont
		       (apply ,throw-cont args)
		       (values-list args))
		   (if ,non-throw-cont
		       (apply ,non-throw-cont args)
		       (values-list args))))
	   (block ,exit
	     (let ((val (multiple-value-list
			    (catch ,tag
			      (return-from ,exit (multiple-value-prog1 ,@body))))))
	       (setf ,caught t)
	       (values-list val))))))))

(defmacro catch-continuation-if (cond-form tag throw-cont non-throw-cont &body body)
  (alexandria:once-only (cond-form throw-cont non-throw-cont)
    `(if ,cond-form
	 (catch-continuation ,tag ,throw-cont ,non-throw-cont ,@body)
	 (if ,non-throw-cont
	     (multiple-value-call ,non-throw-cont (multiple-value-prog1 ,@body))
	     ,@body))))


#| Examples:

(defun ex1 (throw-p)
  (catch-continuation 'wrong
		      (lambda (v w) (format t "Wrong, val: (~a ~a)" v w))
		      (lambda (v w) (format t "Correct, val: (~a ~a)"  v w))
		      (if throw-p
			  (throw 'wrong (values 3 4))
			  (values 1 2))))

(defun ex2 (catch-p throw-p)
  (catch-continuation-if catch-p
      'wrong
      (lambda (v w) (format t "Wrong, val: (~a ~a)" v w))
      (lambda (v w) (format t "Correct, val: (~a ~a)"  v w))
    (if throw-p
	(throw 'wrong (values 3 4))
	(values 1 2))))

|#

