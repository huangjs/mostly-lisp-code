(cl:defpackage #:cps-macro
  (:use #:cl)
  (:export #:cps-macroexpand
	   #:cps-expander
	   #:cps-return
	   
	   #:cps-implicit-progn

	   #:cps-block
	   #:cps-catch
	   #:cps-eval-when
	   #:cps-flet
	   #:cps-function
	   #:cps-go
	   #:cps-if
	   #:cps-label
	   #:cps-let
	   #:cls-let*
	   #:cps-load-time-value
	   #:cps-locally
	   #:cps-macrolet
	   #:cps-multiple-value-call
	   #:cps-multiple-value-prog1
	   #:cps-progn
	   #:cps-progv
	   #:cps-quote
	   #:cps-return-from
	   #:cps-setq
	   #:cps-symbol-macrolet
	   #:cps-tagbody
	   #:cps-the
	   #:cps-throw
	   #:cps-unwind-protect))

(cl:in-package #:cps-macro)

(defun lexenv-p (obj)
  (sb-c::lexenv-p obj))

(defun make-null-lexenv ()
  (sb-c::make-null-lexenv))

(defun copy-lexenv (lexenv)
  (sb-c::make-lexenv :default lexenv))

(defun extend-lexenv (lexenv &key funs vars)
  (sb-c::make-lexenv :default lexenv
		     :vars    vars
		     :funs    funs))

(defmacro get-env (&environment env)
  `(quote ,(list (copy-list (sb-c::lexenv-vars env))
		 (copy-list (sb-c::lexenv-funs env)))))

(defmacro set-env (var &environment env)
  (set var (list (copy-list (sb-c::lexenv-vars env))
		 (copy-list (sb-c::lexenv-funs env))))
  nil)

(defparameter *in-trampoline-p* nil)
(defparameter *recursive-macroexpand-count* 0)
(defparameter *old-macroexpand-hook* *macroexpand-hook*)

(defun count-recursive-macroexpands (fn form env)
  (if (> *recursive-macroexpand-count* 0)
      (let ((*in-trampoline-p* nil)
	    (*macroexpand-hook* *old-macroexpand-hook*))
	(funcall *macroexpand-hook* fn form env))
      (let ((*recursive-macroexpand-count* (1+ *recursive-macroexpand-count*)))
	(funcall *old-macroexpand-hook* fn form env))))

(defparameter *out-env* nil)

(defun env-add-args (lexenv args)
  (let ((*out-env* nil)
	(*macroexpand-hook* *old-macroexpand-hook*)
	(*in-trampoline-p* nil))
    (eval `(locally (declare (sb-ext:muffle-conditions style-warning))
	     (lambda ,args
	       (set-env *out-env*))))
    (destructuring-bind (vars funs)
	*out-env*
      (declare (ignore funs))
      (extend-lexenv lexenv :vars vars))))

(defun env-add-vars (lexenv vars)
  (destructuring-bind (vars funs)
      (let ((*macroexpand-hook* *old-macroexpand-hook*)
	    (*in-trampoline-p* nil))
	(eval `(locally (declare (sb-ext:muffle-conditions style-warning))
		 (let ,vars
		   (declare (ignorable ,@vars))
		   (get-env)))))
    (declare (ignore funs))
    (extend-lexenv lexenv :vars vars)))

(defun env-add-funs (lexenv funs)
  (let ((funs (mapcar (lambda (fun)
			(if (consp fun)
			    (first fun)
			    fun))
		      funs)))
    (destructuring-bind (vars funs)
	(let ((*macroexpand-hook* *old-macroexpand-hook*)
	      (*in-trampoline-p* nil))
	  (eval `(locally (declare (sb-ext:muffle-conditions style-warning))
		   (flet ,(mapcar (lambda (fun)
				    `(,fun ()
					   t))
				  funs)
		     (declare (ignorable ,@(mapcar (lambda (fun)
						     `(function ,fun))
						   funs)))
		     (get-env)))))
      (declare (ignore vars))
      (extend-lexenv lexenv :funs funs))))

(defun env-add-symbol-macros (lexenv symbol-macros)
  (destructuring-bind (vars funs)
      (let ((*macroexpand-hook* *old-macroexpand-hook*)
	    (*in-trampoline-p* nil))
	(eval `(symbol-macrolet ,symbol-macros
		 (get-env))))
    (declare (ignore funs))
    (extend-lexenv lexenv :vars vars)))

(defun env-add-macros (lexenv macros)
  (destructuring-bind (vars funs)
      (let ((*macroexpand-hook* *old-macroexpand-hook*)
	    (*in-trampoline-p* nil))
	(eval `(macrolet ,macros
		 (get-env))))
    (declare (ignore vars))
    (extend-lexenv lexenv :funs funs)))

(defun env-add-declares (lexenv declarations)
  (destructuring-bind (vars funs)
      (let ((*macroexpand-hook* *old-macroexpand-hook*)
	    (*in-trampoline-p* nil))
	(eval `(locally
		   (declare (sb-ext:muffle-conditions style-warning))
		 ,@declarations
		 (get-env))))
    (extend-lexenv lexenv :vars vars :funs funs)))

(defparameter *continuation* nil)
(defparameter *environment* nil)

(defun cps-call/cc (receiver)
  (let* ((cc *continuation*)
	 (reified-cc (lambda (val)
		       (setf *continuation* cc)
		       (cps-return val))))
    (funcall receiver reified-cc)))

(defmacro cps-reset (&body body)
  (let ((_cc (gensym "CC")))
    `(let ((,_cc *continuation*))
       (list 'symbol-macrolet (list (list 'shift-delimiter ,_cc))
	     ,@body))))

(defun cps-shift (receiver)
  (let* ((cc *continuation*))
    (multiple-value-bind (limit limitp)
	(macroexpand-1 'shift-delimiter *environment*)
      (unless limitp
	(setf limit nil))
      (setf *continuation* limit)
      (loop for sub-cc on cc
	 until (eq sub-cc limit)
	 collect (first sub-cc) into fragment
	 finally (let* ((fragment (if limitp
				      (butlast fragment)
				      fragment))
			(reified-fragment (lambda (val k)
					    (setf *continuation*
						  (append fragment
							  (cons (cons k *environment*)
								*continuation*)))
					    (cps-return val))))
		   (return (funcall receiver reified-fragment)))))))

(defun cps-macroexpand (form &optional (env *environment*) subcont)
  (if *in-trampoline-p*
      (throw 'bind
	(list form
	      (if (lexenv-p env)
		  (copy-lexenv env)
		  env)
	      subcont))
      (let ((*in-trampoline-p* t)
	    (*recursive-macroexpand-count* 0)
	    (*old-macroexpand-hook* *macroexpand-hook*)
	    (*macroexpand-hook*  'count-recursive-macroexpands))
	(loop
	   with *continuation* = (if subcont
				     (list (cons subcont
						 env))
				     nil)
	    and form = form
	    and  env = (if env
			   (copy-lexenv env)
			   (make-null-lexenv))
	   do (block inner
		(destructuring-bind (new-form new-env subcont)
		    (catch 'bind
		      (let ((ret (do-expansion form env)))
			(if (null *continuation*)
			    (return ret)
			    (setf form 'call
				  env  (cons (pop *continuation*)
					     ret)))
			(return-from inner)))
		  (setf form            new-form
			env             new-env
			*continuation* (if subcont
					   (cons (cons subcont
						       env)
						 *continuation*)
					   *continuation*))))))))

(define-symbol-macro cps-expander expand-form)

(defun expand-form (form env)
  (if (atom form)
      (macroexpand-1 form env)
      (multiple-value-bind (form expandedp)
	  (macroexpand-1 form env)
	(if expandedp
	    (values form expandedp)
	    (labels ((self (todo done)
		       (if (null todo)
			   (reverse done)
			   (cps-bind ((first todo)
				      result
				      env)
			     (self (rest todo)
				   (cons result
					 done))))))
	      (self (rest form)
		    (list (first form))))))))

(defun do-expansion (form env)
  (flet ((dispatch (name args)
	   (let ((*environment* env))
	     (apply (macroexpand-1 name env)
		    env
		    args)))
	 (expand (form)
	   (let ((*environment* env)
		 (handler (macroexpand-1 'cps-expander env)))
	     (multiple-value-bind (new-form morep)
		 (funcall handler form env)
	       (if morep
		   (throw 'bind (list new-form env nil))
		   new-form)))))
    (cond ((eq form 'call)
	   (destructuring-bind ((target . env) . arg)
	       env
	     (let ((*environment* env))
	       (funcall target arg))))
	  ((atom form)
	   (expand form))
	  (t (destructuring-bind (head . args)
		 form
	       (let ((target (case head
			       ((block) 'cps-block)
			       ((catch) 'cps-catch)
			       ((eval-when) 'cps-eval-when)
			       ((flet)  'cps-flet)
			       ((function) 'cps-function)
			       ((go)    'cps-go)
			       ((if)    'cps-if)
			       ((labels) 'cps-labels)
			       ((let)   'cps-let)
			       ((let*)  'cps-let*)
			       ((load-time-value)
				'cps-load-time-value)
			       ((locally) 'cps-locally)
			       ((macrolet) 'cps-macrolet)
			       ((multiple-value-call)
				'cps-multiple-value-call)
			       ((multiple-value-prog1)
				'cps-multiple-value-prog1)
			       ((progn) 'cps-progn)
			       ((progv) 'cps-progv)
			       ((quote) 'cps-quote)
			       ((return-from)
				'cps-return-from)
			       ((setq)  'cps-setq)
			       ((symbol-macrolet)
				'cps-symbol-macrolet)
			       ((tagbody) 'cps-tagbody)
			       ((the)   'cps-the)
			       ((throw) 'cps-throw)
			       ((unwind-protect)
				'cps-unwind-protect)
			       (otherwise nil))))
		 (if target
		     (dispatch target args)
		     (expand form))))))))

(defun cps-return (value &optional (env *environment*))
  (cps-macroexpand 'call
		   (cons (cons #'identity env)
			 value)
		   nil))

(defmacro cps-bind ((form &optional (name form) (env '*environment*))
		    &body body)
  (assert (symbolp name))
  `(cps-macroexpand ,form
		    ,env
		    (lambda (,name)
		      ,@body)))

(defun cps-implicit-progn (env body cont)
  (let* ((end-declares (or (position-if-not
			    (lambda (form)
			      (and (consp form)
				   (eq (first form)
				       'declare)))
			    body)
			   (length body)))
	 (declares     (subseq body 0 end-declares))
	 (values       (subseq body end-declares))
	 (env          (env-add-declares env declares)))
    (labels ((self (todo done)
	       (if (null todo)
		   (funcall cont
			    (append declares
				    (reverse done)))
		   (cps-bind ((first todo) value env)
		     (self (rest todo)
			   (cons value done))))))
      (self values nil))))

(defun cps-block (env name &rest body)
  (cps-implicit-progn env
		      body
		      (lambda (forms)
			`(block ,name
			   ,@forms))))

(defun cps-catch (env name &rest body)
  (cps-bind (name)
    (cps-implicit-progn env
			body
			(lambda (forms)
			  `(catch ,name
			     ,@forms)))))

(defun cps-eval-when (env situations &rest body)
  (cps-implicit-progn env
		      body
		      (lambda (forms)
			`(eval-when ,situations
			   ,@forms))))

(defun cps-flet (env functions &rest body)
  (labels ((self (todo done)
	     (if (null todo)
		 (cps-implicit-progn env
				     body
				     (lambda (forms)
				       `(flet ,(reverse done)
					  ,@forms)))
		 (destructuring-bind (name args &body fn-body) ;;fixme
		     (first todo)
		   (cps-implicit-progn (env-add-args env args)
				       fn-body
				       (lambda (fn-body)
					 (self (rest todo)
					       (cons `(,name ,args ,@fn-body)
						     done))))))))
    (self functions nil)))

(defun cps-function (env name)
  (declare (ignore env))
  `(function ,name))

(defun cps-go (env label)
  (declare (ignore env))
  `(go ,label))

(defun cps-if (env cond yes &optional no)
  (declare (ignore env))
  (cps-bind (cond)
    (cps-bind (yes)
      (cps-bind (no)
	`(if ,cond ,yes ,no)))))

(defun cps-labels (env functions &rest body)
  (let ((env (env-add-funs env (mapcar #'first functions))))
    (labels ((self (todo done)
	       (if (null todo)
		   (cps-implicit-progn env
				       body
				       (lambda (body)
					 `(labels ,(reverse done)
					    ,@body)))
		   (destructuring-bind (name args &body fn-body) ;; fixme
		       (first todo)
		     (cps-implicit-progn (env-add-args env args)
					 fn-body
					 (lambda (fn-body)
					   (self (rest todo)
						 (cons `(,name ,args ,@fn-body)
						       done))))))))
      (self functions nil))))

(defun cps-let (env bindings &rest body)
  (labels ((self (todo done)
	     (if (null todo)
		 (cps-implicit-progn (env-add-vars env
						   (mapcar (lambda (binding)
							     (if (consp binding)
								 (first binding)
								 binding))
							   bindings))
				     body
				     (lambda (body)
				       `(let ,(reverse done)
					  ,@body)))
		 (destructuring-bind (next . todo)
		     todo
		   (if (atom next)
		       (self todo (cons next done))
		       (cps-bind ((second next) value env)
			 (self todo (cons (list (first next)
						value)
					  done))))))))
    (self bindings nil)))

(defun cps-let* (env bindings &rest body)
  (labels
      ((self (todo done env)
	 (if (null todo)
	     (cps-implicit-progn env
				 body
				 (lambda (body)
				   `(let* ,(reverse done)
				      ,@body)))
	     (destructuring-bind (binding . todo)
		 todo
	       (if (atom binding)
		   (self todo
			 (cons binding done)
			 (env-add-vars env `(,binding)))
		   (cps-bind ((second binding) value env)
		     (self todo
			   (cons (list (first binding)
				       value)
				 done)
			   (env-add-vars env `(,(first binding))))))))))
    (self bindings nil env)))

(defun cps-load-time-value (env value &optional read-only-p)
  (declare (ignore env))
  (cps-bind (value)
    `(load-time-value ,value ,read-only-p)))

(defun cps-locally (env &rest body)
  (cps-implicit-progn env body (lambda (body)
				 `(locally ,@body))))

(defun cps-macrolet (env definitions &rest body)
  (cps-implicit-progn (env-add-macros env
				      definitions)
		      body
		      (lambda (body)
			`(macrolet ,definitions
			   ,@body))))

(defun cps-multiple-value-call (env fn &rest values)
  (cps-implicit-progn env
		      `(,fn ,@values)
		      (lambda (exprs)
			`(multiple-value-call ,@exprs))))

(defun cps-multiple-value-prog1 (env form &rest forms)
  (cps-implicit-progn env
		      `(,form ,@forms)
		      (lambda (forms)
			`(multiple-value-prog1 ,@forms))))

(defun cps-progn (env &rest body)
  (assert (not (and (consp (first body))
		    (eq (first (first body))
			'declare))))
  (cps-implicit-progn env
		      body
		      (lambda (forms)
			`(progn ,@forms))))

(defun cps-progv (env symbols vals &rest body)
  (cps-bind (symbols)
    (cps-bind (vals)
      (cps-implicit-progn env
			  body
			  (lambda (forms)
			    `(progv ,symbols
				 ,vals
			       ,@forms))))))

(defun cps-quote (env value)
  (declare (ignore env))
  `(quote ,value))

(defun cps-return-from (env tag &optional value)
  (declare (ignore env))
  (cps-bind (value)
    `(return-from ,tag ,value)))

(defun cps-setq (env &rest pairs)
  (declare (ignore env))
  (labels ((self (todo done)
	     (if (null todo)
		 `(setq ,@(mapcan #'copy-list
				  (reverse done)))
		 (destructuring-bind (var form . todo)
		     todo
		   (cps-bind (form)
		     (self todo
			   (cons (list var form)
				 done)))))))
    (self pairs nil)))

(defun cps-symbol-macrolet (env definitions &rest body)
  (cps-implicit-progn (env-add-symbol-macros env definitions)
		      body
		      (lambda (forms)
			`(symbol-macrolet ,definitions
			   ,@forms))))

(defun cps-tagbody (env &rest body)
  (declare (ignore env))
  (labels ((self (todo done)
	     (if (null todo)
		 `(tagbody ,@(reverse done))
		 (destructuring-bind (statement . todo)
		     todo
		   (if (atom statement)
		       (self todo (cons statement done))
		       (cps-bind (statement)
			 (self todo
			       (cons statement
				     done))))))))
    (self body nil)))

(defun cps-the (env type form)
  (declare (ignore env))
  (cps-bind (form)
    `(the ,type ,form)))

(defun cps-throw (env tag result)
  (declare (ignore env))
  (cps-bind (tag)
    (cps-bind (result)
      `(throw ,tag ,result))))

(defun cps-unwind-protect (env protected &rest cleanup)
  (assert (not (and (consp protected)
		    (eq (first protected)
			'declare))))
  (cps-implicit-progn env
		      `(,protected ,@cleanup)
		      (lambda (forms)
			`(unwind-protect ,@forms))))