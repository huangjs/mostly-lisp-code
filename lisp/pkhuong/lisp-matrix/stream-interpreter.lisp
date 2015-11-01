(cl:defpackage "CA.PVK.STREAM-INTERP"
  (:use #:cl))

(cl:in-package "CA.PVK.STREAM-INTERP")

(defstruct closure
  args body env)

(defstruct receiver
  arg body env)

(defstruct state
  expr env continuation)

(defun do-return (value continuation)
  (if (null continuation)
      (throw 'return value)
      (let* ((receiver (first continuation))
	     (continuation (rest continuation))
	     (name (receiver-arg receiver)))
	(make-state :expr (receiver-body receiver)
		    :env  (acons name value
				 (receiver-env receiver))
		    :continuation continuation))))

(defun make-bind (value-expr receiver env continuation)
  (make-state :expr value-expr
	      :env  env
	      :continuation (cons receiver
				  continuation)))

(defun lookup (name closure)
  (let ((entry (assoc name (closure-env closure))))
    (if entry
	(cdr entry)
	(error "Unbound variable: ~A" name))))

(defun do-lambda (expr env continuation)
  (destructuring-bind (lambda args &rest body)
      expr
    (declare (ignore lambda))
    (do-return (make-closure :args args :body `(progn ,@body) :env env)
               continuation)))

(defun do-apply (expr env continuation)
  (let ((fun  (first expr))
	(args (rest expr)))
    (if (and (typep fun 'closure)
	     (every 'atom args))
	(make-state :expr (closure-body fun)
		    :env  (append (mapcar (lambda (name var)
					    (cons name
						  (cdr (assoc var env))))
					  (closure-args fun)
					  args)
				  (closure-env fun))
		    :continuation continuation)
	(let ((to-eval nil)
	      (tmp     (gensym "ARG")))
	  (labels ((inner (expr)
		     (cond ((null expr) nil)
			   ((consp (first expr)) (setf to-eval (first expr))
			                         (cons tmp     (rest expr)))
			   (t                    (cons (first expr)
						       (inner (rest expr)))))))
	    (let ((expr (inner expr)))
	      (make-state :expr to-eval
			  :env env
			  :continuation (cons (make-receiver :arg tmp :body expr :env env)
					      continuation))))))))