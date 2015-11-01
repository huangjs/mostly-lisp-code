(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel) 
  (defvar *form-handler* (make-hash-table))

  (defun force-first (x)
    (if (listp x) (first x) x))
  (defun force-list (x)
    (if (listp x) x (list x))))

(defvar *env*)

(defun binding-to-symbol (binding)
  (let ((name (force-first binding)))
    (cond ((listp name)
	   (assert (eq 'setf (first name)))
	   (check-type (second name) symbol)
	   (second name))
	  (t
	   name))))

(defmacro with-imposed-bindings (&body body)
  `(locally ,@body)
  #+sbcl
  (destructuring-bind ((binder bindings &rest binder-body))
      body
    `(locally
	 (declare (sb-ext:disable-package-locks ,@(mapcar 'binding-to-symbol bindings)))
       (,binder ,bindings 
		,@binder-body))))

(defmacro without-package-locking (&body body)
  `(
    #. (progn 'progn
	      #+sbcl 'sb-ext:without-package-locks)
       ,@body))

(defmacro defhandler (symbol lambda-list &body body)
  (let ((syms (force-list symbol)))
    (let ((func (intern (format nil "~A~A" 'hander- (first syms)))))
      `(progn
	 (defun ,func ,lambda-list
	   ,@body)
	 (setf
	  ,@(loop for sym in syms
		  collect `(gethash ',sym *form-handler*)
		  collect `',func))))))

(defun e-list (list)
  (mapcar 'e list))

(defhandler (progn locally) (progn &rest body)
  `(list ',progn
	 ,@(e-list body)))


(defhandler let (let bindings &rest body)
  (let ((names (loop for binding in bindings 
		     collect 
		  (force-first binding))))
    `(list*
      ',let
      (list 
       ,@(loop for binding in bindings
	       collect 
	    (if (symbolp binding)
		`,'binding
		`(list ',(first binding)
		       ,@(e-list (rest binding))))))
      (with-imposed-bindings
	  (,let ,names
	    (declare (ignorable ,@names))
	    (m-list ,@body))))))


(defun dump-fbinding (name lambda-list &rest body)
  (let (bound-vars)
    (labels (
	     (binding-vars (&rest body)
	       `(let ,bound-vars
		  (declare (ignorable ,@bound-vars))
		  (m-list ,@body)))
	     (l (lambda-arg)
	       (cond ((member lambda-arg lambda-list-keywords)
		      `',lambda-arg)
		     (t
		      (destructuring-bind
			    (var &optional (val nil val-present-p) present-var)
			  (force-list lambda-arg)
			
			(prog1
			    (if (listp lambda-arg)
				`(list ',var (car ,@(when val-present-p `(,(binding-vars val)))) 
				       ,@(when present-var `(',present-var)))
				`',var)
			  (push var bound-vars)
			  (when present-var (push present-var bound-vars))))))))

      `(list* ',name (list ,@(mapcar #'l lambda-list))
	      ,(apply #'binding-vars body)))))

(defun dump-fbindings (bindings)
  `(list ,@(mapcar (lambda (f) (apply 'dump-fbinding f)) bindings)))

(defun declare-fbindings-ignorable (bindings)
  `(declare (ignorable ,@(mapcar (lambda (f)
				   `(function ,(force-first f))) bindings))))

(defun maybe-locally (forms)
  (flet ((starts-with-declare ()
	   (and (listp (first forms)) (eq (first (first forms)) 'declare))))
    (cond ((or (rest forms) (starts-with-declare))
	   (list* (if (starts-with-declare) 'locally 'progn) forms))
	  (t
	   (first forms)))))

(defhandler declare (declare &rest body)
  `(list ',declare 
	 ,@(mapcar (lambda (f) `',f) body)))

(defhandler block (block name &rest body)
  `(list ',block ',name
	 ,@(e-list body)))

(defhandler return-from (return-from name &optional (value nil value-p))
  `(list ',return-from ',name
	 ,@(when value-p
	     `(,(e value)))))

(defhandler catch (catch tag &rest body)
  `(list ',catch ,(e tag) ,@(e-list body)))

(defhandler load-time-value (load-time-value form &optional (read-only-p nil rop-p))
  `(list ',load-time-value ,(e form) 
	 ,@(when rop-p
	     `(',read-only-p))))

(defhandler 
    (macrolet 
	symbol-macrolet 
      compiler-let ; mostly for Lispworks
      ) 
    (macrolet bindings &rest body)
  `(maybe-locally
    (with-imposed-bindings
	(,macrolet ,bindings
					;	    ,(declare-fbindings-ignorable bindings) ;; this does not work for macrolet and causes warnings
	  (m-list ,@body)))))

(defhandler flet (flet bindings &rest body)
  `(list* ',flet
	  ,(dump-fbindings bindings)
	  (with-imposed-bindings
	      (,flet ,bindings
		,(declare-fbindings-ignorable bindings)
		(m-list ,@body)))))

(defhandler labels (labels bindings &rest body)
  `(with-imposed-bindings
       (,labels ,bindings
	 ,(declare-fbindings-ignorable bindings)
	 (list* ',labels
		,(dump-fbindings bindings)
		(m-list ,@body)))))

(defhandler let* (let* bindings &rest body)
  (if (not bindings)
      (e `(locally ,@body))
      (destructuring-bind (first &rest rest)
	  bindings
	(e `(let (,first)
	      ,@(if rest
		    `((,let* ,rest ,@body))
		    body))))))

(defhandler eval-when (eval-when situation &rest body)
  `(list ',eval-when ',situation
	 ,@(e-list body)))

#+sbcl
(defhandler sb-int:named-lambda (named-lambda name lambda-list &rest body)
  `(list* ',named-lambda ,(apply 'dump-fbinding name lambda-list body)))

(defhandler defun (defun name lambda-list &rest body)
  `(list* ',defun ,(apply 'dump-fbinding name lambda-list body)))


(defhandler lambda (lambda lambda-list &rest body)
  (apply 'dump-fbinding lambda lambda-list body))

(defun tagbody-restore-tags (list)
  (loop for f in list
	collect 		    
     (cond ((or (symbolp f) (integerp f))
	    `(progn ,f))
	   ((and (listp f) (eq 'tagbody-restore-tag (first f)))
	    (second f))
	   (t
	    f))))

(defhandler tagbody (tagbody &rest tags-and-forms)
  `(list* ',tagbody
	  (tagbody-restore-tags 
	   (list
	    ,@(loop for f in tags-and-forms 
		    collect
		 (if (or (symbolp f) (integerp f))
		     `(list 'tagbody-restore-tag ',f)
		     (e f)))))))

(defun maybe-setf (pairs)
  `(,(if (loop for s in pairs by #'cddr
	       thereis (listp s))
	 'setf
	 'setq)
     ,@pairs))

(defhandler setq (setq &rest pairs)
  (declare (ignore setq))
  `(maybe-setf
    (list ,@(e-list pairs))))

(defhandler function (function name)
  `(list ',function
	 ,(if (symbolp name)
	      `',name
	      (e name))))

(defhandler the (the value-type form)
  `(list ',the ',value-type ,(e form)))

(defhandler go (go tag)
  `(list ',go ',tag))

(defhandler unwind-protect (unwind-protect protected-form &rest cleanup)
  `(list ',unwind-protect ,(e protected-form) ,@(e-list cleanup)))

(defhandler progv (progv symbols values &rest body)
  `(list ',progv
	 (list ,@(e-list symbols))
	 (list ,@(e-list values))
	 ,@(e-list body)))

(defhandler quote (quote object)
  `(list ',quote ',object))

(defun default-form-handler (first &rest rest)
  `(list ,(if (symbolp first)
	      `',first
	      (e first)) ,@(e-list rest)))

(defun form-handler (first)
  (gethash first *form-handler*
	   'default-form-handler))

(defun e (form)
  (flet ((handle (form)
	   (apply (form-handler (first form)) form)))
    (cond ((and (listp form) (gethash (first form) *form-handler*))
	   (handle form))
	  (t
	   (multiple-value-bind (form expanded)
	       (macroexpand-1 form *env*)
	     (cond (expanded
		    (e form))
		   (t
		    (typecase form
		      (null nil)
		      (list 
			 (handle form))
		      (t
			 `',form)))))))))

(defmacro m (form &environment *env*)
  (e form))

(defmacro m-list (&body body &environment *env*)
  `(list ,@(e-list body)))

(defun macroexpand-dammit (form &optional *env*)
  (eval (e form)))
