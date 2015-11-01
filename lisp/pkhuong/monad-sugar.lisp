(defpackage #:monad-syntax
  (:nicknames #:m)
  (:export #:monad #:build #:with-monad

	   #:zero  #:%zero
	   #:plus #:sum
	   
	   #:bind  #:seq   #:return
	   #:>>=   #:>>
	   #:%return

	   #:lambda #:lam  #:%scope #:%var
	   #:call ;;~ap
	   
	   #:lift1 #:lift  

	   #:progn #:let*
	   #:fns
	   #:flet  #:labels

	   #:filter #:map #:fold ;;no need for fold?
	   #:guard
	   #:join   #:sequence
	   #:unless #:when))

(defpackage #:monad-code
  (:use #:cl))

(in-package "MONAD-CODE")

(defclass m:monad ()
  ())

(defgeneric m:build (type-name &key &allow-other-keys)
  (:documentation
   "Specialise on the symbol naming the class
    return an object of that type"))

(defmethod m:build ((name symbol) &key &allow-other-keys)
  (make-instance name))

(defmethod m:build ((class-obj class) &key &allow-other-keys)
  (make-instance class-obj))

(defparameter *current-monad* (m:build 'm:monad)
  "an instance of the current monad")

(defmacro m:with-monad (monad-name &body body)
  `(let ((*current-monad* (m:build ',monad-name)))
     (declare (special *current-monad*))
     ,@body))

(define-symbol-macro m:zero (m:%zero *current-monad*))

(defgeneric m:%zero (dispatch)
  (:documentation "Returns the zero for type of dispatch monad"))

(defgeneric m:plus (a b)
  (:documentation "Only defined for some types (those that implement m:%zero)."))

(defun m:sum (elements)
  (reduce #'m:plus elements :initial-value m:zero))

(defgeneric m:bind (mvalue consumer &key &allow-other-keys)
  (:documentation
   "Gets the value from the monad mvalue passes it to consumer"))

(defmethod m:bind :around (mvalue consumer &key &allow-other-keys)
  "Bind is (>>=) :: m a -> (a -> m b) -> m b "
  (declare (ignore consumer))
  (let ((*current-monad* mvalue))
    (call-next-method)))

(defun m:>>= (mvalue consumer)
  (m:bind mvalue consumer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro m:seq (mvalue next &rest options)
    (let ((_ (gensym "IGNORE")))
      `(m:bind ,mvalue (m:lam (,_)
			 ,next)
	       ,@options)))

  (defmacro m:>> (mvalue next)
    `(m:seq ,mvalue ,next))
  )

(defgeneric m:%return (dispatch value &key &allow-other-keys)
  (:documentation
   "Return a monad of the same as dispatch containing value"))

(defun m:return (value &rest other-args)
  "Wraps value in a monad of the same type as *current-monad*"
  (apply #'m:%return *current-monad* value other-args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *lam-bodies* ()
    "alist of lambda-uid -> (source &optional closure)")

  (define-symbol-macro current-local-vars nil)

  (define-symbol-macro current-local-functions nil)
  
  (defmacro m:fns ((&rest fns) &body body &environment env)
    (let* ((old-fns (macroexpand-1 'current-local-functions env))
	   (remaining (set-difference old-fns fns :key #'first))
	   (all-fns (append fns remaining)))
      `(symbol-macrolet ((current-local-functions ,all-fns))
	 (macrolet ,(mapcar (lambda (fn)
			      (destructuring-bind (name arity)
				  fn
				(let* ((arity (if (integerp arity)
						  arity
						  (length arity)))
				       (args (loop repeat arity
						collect (gensym "ARG"))))
				  `(,name ,args (list 'm:call ',name ,@args)))))
			    all-fns)
	   ,@body))))
  
  ;; Should memoize on arg, body & environment.
  ;; (assume top-level environment wrt macrolet, etc)

  (defparameter *running-code* nil)
  
  (defmacro m:lam ((arg) &body body &environment env)
    (let* ((current-fns  (macroexpand-1 'current-local-functions env))
	   (current-vars (cons arg (remove arg (macroexpand-1 'current-local-vars env))))
	   (uid (or (first (find-if (lambda (entry)
				      (destructuring-bind (uid vars fns bodyp)
					  entry
					(declare (ignore uid))
					(and (equal vars current-vars)
					     (equal fns  current-fns)
					     (equal body bodyp))))
				    *lam-bodies*))
		    (unless *running-code*
		      (gensym "LAMBDA-UID"))))
	   (bodyp `(lambda (,arg)
		     (declare (ignorable ,arg))
		     (symbol-macrolet ((current-local-vars ,current-vars))
		       ,@body))))
      (assert uid)
      (unless *running-code*
	(pushnew (list uid current-vars current-fns body) *lam-bodies*
		 :key #'first))
      `(m:%scope *current-monad* ',arg ',uid ,bodyp)))
  )

(defgeneric m:%scope (dispatch arg-name uid closure)
  (:documentation "Returns a funcallable object for closure (with uid), and argument arg-name"))

(defmethod m:%scope (dispatch arg-name uid closure)
  closure)

(defgeneric m:%var (dispatch val name) ;;not sure if it's needed
  (:documentation "Looks up the value for name in val or monad dispatch"))

(defgeneric (setf m:%var) (new-val dispatch val name)) ;;same here

;;Check if ok w/ serialised closures, etc

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro m:lambda ((arg &rest args) &body body) ;;FIXME rename. lamc?
    (labels ((inner (args)
	       (if (null (rest args))
		   `(m:lam (,(first args))
		      ,@body)
		   `(m:lam (,(first args))
		      ,(inner (rest args))))))
      (inner (cons arg args))))

  (defmacro m:call (fn arg &rest args)
    (labels ((inner (args)
	       (if (null (rest args))
		   `(m:bind (m:return ,(first args))
			    ,fn)
		   `(m:bind (m:return ,(first args))
			    ,(inner (rest args))))))
      (inner (reverse (cons arg args)))))
  )

(defun m:lift1 (fn)
  "Take a function of 1 value and 
   returns a function of 1 monad"
  (m:lam (mvalue)
    (m:bind mvalue (m:lam (value)
		     (m:%return mvalue (funcall fn value))))))

(eval-when  (:compile-toplevel :load-toplevel :execute)

  (defmacro m:lift (n) ;;FIXME how should this work!?
    "Takes a function that takes a list of values and 
   returns a function that takes a list of *monad*"
    (assert (and (integerp n)
		 (> n 0)))
    (let ((fn (gensym "FN")))
      (labels ((inner (n args)
		 (let ((_ (gensym "ARG"))
		       (__ (gensym "TMP")))
		   (if (= n 1)
		       `(m:lam (,_)
			  (m:bind ,_
				  (m:lam (,__)
				    (m:%return ,_ (m:call ,fn ,__ ,@(reverse args))))))  ;;can subtype?
		       `(m:lam (,_)
			  (m:bind ,_
				  (m:lam (,__)
				    (m:call ,(inner (1- n) (cons __ args))))))))))
	`(m:lam (,fn)
	   ,(inner n nil)))))
  
  (defmacro m:progn (form &rest forms)
    (labels ((inner (forms)
	       (if (null (rest forms))
		   (first forms)
		   `(m:seq ,(first forms)
			   ,(inner (rest forms))))))
      (inner (cons form forms))))

  (defmacro m:let* ((&rest bindings) &body body)
    (labels ((inner (bindings)
	       (if (null bindings)
		   `(m:progn ,@body)
		   (destructuring-bind (current &rest next)
		       bindings
		     (if (atom current)
			 `(m:bind nil
				  (m:lam (,current)
				    ,(inner next)))
			 (destructuring-bind (name value)
			     current
			   `(m:bind ,value
				    (m:lam (,name)
				      ,(inner next)))))))))
      (inner bindings)))

  (defmacro m:flet ((&rest bindings) &body body)
    `(m:let* ,(mapcar (lambda (binding)
			(destructuring-bind (name (&rest args) &body fn-body)
			    binding
			  `(,name (m:return (m:lambda ,args
					      ,@fn-body)))))
		      bindings)
       (m:fns ,(mapcar (lambda (binding)
			 (destructuring-bind (name (&rest args) &body fn-body)
			     binding
			   (declare (ignore fn-body))
			   `(,name ,args)))
		       bindings)
	 ,@body)))

  (defmacro m:labels ((&rest definitions) &body body)
    (let ((fn-entries (mapcar (lambda (definition)
				(destructuring-bind (name args &body body)
				    definition
				  (declare (ignore body))
				  (list name args)))
			      definitions))
	  (tmps (loop for i in definitions collect (gensym "RFN"))))
      `(m:let* ,(mapcar (lambda (definition _)
			  (destructuring-bind (name args &body fn-body)
			      definition
			    (declare (ignore name))
			    `(,_ (m:return (m:lambda ,(append tmps args)
					     (m:let* ,(mapcar (lambda (fn-entry _)
								(destructuring-bind (name args)
								    fn-entry
								  `(,name (m:return
									    (m:lambda ,args
									      (m:call ,_ ,@tmps ,@args))))))
							      fn-entries tmps)
					       (m:fns ,fn-entries
						 ,@fn-body)))))))
			definitions tmps)
	 (m:flet ,(mapcar (lambda (fn-entry _)
			    (destructuring-bind (name args)
				fn-entry
			      `(,name ,args (m:call ,_ ,@tmps ,@args))))
			  fn-entries tmps)
	   ,@body))))
  )

(defun m:filter (pred list)
  (if (null list)
      (m:return nil)
      (m:let* ((bool  (m:call   pred (first list)))
	       (rest  (m:filter pred (rest list))))
	(if bool
	    (m:return (cons (first list)
			    rest))
	    (m:return rest)))))

(defgeneric m:map (fn mvalue)
  (:documentation
   "fmap. m:map id = id, m:map (f . g) = m:map f . m:map g
    Lets fn operate on mvalue even though it was never designed 
    to work w/ that monad (unwrap, call, wrap)"))

(defun m:guard (truth-value)
  (if truth-value
      (m:return ())
      m:zero))

(defun m:join (x)
  (m:bind x (m:lam (x) x)))

(defun m:sequence (list)
  (if (null list)
      (m:return ())
      (m:let* ((head (first list))
	       (rest (m:return (m:sequence (rest list)))))
	(m:return (cons head rest)))))

(defun m:unless (truth-value action)
  (if truth-value
      (m:return ())
      action))

(defun m:when (truth-value action)
  (if truth-value
      action
      (m:return ())))

(defmethod m:build ((name (eql 'list)) &key &allow-other-keys)
  (list nil))

(defmethod m:%zero ((dispatch list))
  nil)

(defmethod m:plus ((a list) (b list))
  (append a b))

(defmethod m:bind ((mvalue list) consumer &key &allow-other-keys)
  (reduce #'append (let ((*current-monad* '(nil)))
		     (mapcar consumer mvalue))
	  :initial-value nil))

(defmethod m:%return ((dispatch list) value &key &allow-other-keys)
  (list value))

(defclass serial (m:monad)
  ((value :accessor value-of :initarg :value :initform nil)))

(defclass serial-closure ()
  ((body :reader body-of :initarg :body)
   (name :reader name-of :initarg :name)
   (env  :reader env-of  :initarg :env)))

(defmethod print-object ((obj serial-closure) stream)
  (format stream "#<SERIAL-CLOSURE NAME: ~A ENV: ~A>"
	  (name-of obj)
	  (env-of  obj)))

(defparameter *current-env* nil)

(defmethod m:%scope ((dispatch serial) arg uid closure)
  (declare (ignore dispatch arg closure))
  (destructuring-bind (uid vars fns body &optional closure)
      (assoc uid *lam-bodies* :test #'eq)
    (if closure
	(make-instance 'serial-closure
		       :body closure
		       :name uid
		       :env  *current-env*)
	(progn
	  (let* ((*running-code* t)
		 (closure (eval `(lambda ,vars
				   (symbol-macrolet ((current-local-vars ,vars)
						     (current-local-functions ,fns))
				     (m:fns ,fns
				       ,@body))))))
	    (setf (cdr (assoc uid *lam-bodies*))
		  (list vars fns body closure))
	    (make-instance 'serial-closure
			   :body closure
			   :name uid
			   :env *current-env*))))))

(defmethod m:bind ((mvalue serial) (consumer serial-closure) &key &allow-other-keys)
  (let ((*current-monad* mvalue)
	(*current-env* (cons (value-of mvalue)
			     (env-of consumer))))
    (apply (body-of consumer) *current-env*)))

(defmethod m:%return ((dispatch serial) value &key &allow-other-keys)
  (make-instance 'serial :value value))