(cl:in-package #:template-lisp)

;;;""" delimited strings cribbed from cinline (http://homepages.nyu.edu/~ys453/)

(let ((normal-string-reader (get-macro-character #\")))
  (declare (type function normal-string-reader))
  (defun read-multiline-string (stream c)
    (let ((buffer ()))
      (when (not (char= #\" (peek-char nil stream)))
	(return-from read-multiline-string
	  (funcall normal-string-reader stream c)))
      (read-char stream)

      (when (not (char= #\" (peek-char nil stream)))
	(return-from read-multiline-string
	  ""))
      (read-char stream)

      (do ((chars (list (read-char stream)
			(read-char stream)
			(read-char stream))
		  (cdr (nconc chars (list (read-char stream))))))
	  ((every #'(lambda (c) (eq c #\")) chars)
	   (coerce (nreverse buffer) 'string))
	(push (car chars) buffer)))))

;;use inside (eval-when (:execute :load-toplevel :compile-toplevel) ...)

(defun enable-raw-string-syntax ()
  (set-macro-character #\" #'read-multiline-string))

;;Note, check when need to specify typename, tempalte, etc.
;;grep for "::" for a quick check

(defparameter *symbol-name* '((cons  . "Cons")
			      (1+    . "One_plus")
			      (1-    . "One_minus")
    			      (+     . "Add")
			      (-     . "Sub")
			      (*     . "Mul")
			      (/     . "Div")
			      (%     . "Mod")
			      (^     . "Logxor")
			      (&     . "Logand")
			      (\|    . "Logior")
			      (~     . "Lognot")
			      (!     . "Not")
			      (>     . "Gt")
			      (<     . "Lt")
			      (>>    . "Sar")
			      (<<    . "Sal")
			      (&&    . "And")
			      (\|\|  . "Or")
			      (equal . "Equal")
			      (=     . "Equal")
			      (cdr   . "Cdr")
			      (car   . "Car")
			      (nil   . "False")
			      (T     . "True"))
  "list of assoc symbol -> C++ name for functions")
(defvar *in-template* nil)
(defvar *body* nil)
(defvar *symbols* nil)
(defvar *toplevel*)

(defun set-symbol (symbol &optional value)
  (cond ((and (null value)
	      (null (assoc symbol *symbols*))
	      (null (rassoc (real-name symbol) *symbol-name* :test #'string=)))
	 (push (cons symbol value) *symbols*))
	(value
	 (setf *symbols* (cons (cons symbol value)
			       (remove-if (lambda (entry)
					    (equal symbol (car entry)))
					  *symbols*)))))
  *symbols*)

(defun dash-to-underscore (string)
  (substitute #\_ #\- string))

(defun first-alpha (string)
  (position-if (lambda (char)
		 (position char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
	       string))

(defun capitalize (string)
  (let ((pos (first-alpha string)))
      (string-upcase string :start pos :end (1+ pos))))

(defun real-name (identifier)
  (or (cdr (assoc identifier *symbol-name* :test #'equal))
      (to-string identifier :pretty t)))

(defun translate (form)
  (etypecase form
    (integer (make-integer form)) ;;literals
    ((member T nil) (make-bool form))
    ((or symbol string) (make-literal form))
    (list (case (car form) ;;special forms
	    ((defun) (if *toplevel*
			 (make-defun form)
			 (error "defun in non-toplevel place [~A]" form)))
	    ((define) (if *toplevel*
			  (make-define form)
			  (error "define in non-toplevel place [~A]" form)))
	    ((if) (make-if form))
	    ((lambda) (make-lambda form))
	    ((let) (make-let form))
	    ((quote) (make-quote form))
	    ((out) (make-out form))
	    (t (make-call form))))))

;;add letrec.

(defun define-symbol (entry)
  (destructuring-bind (symbol . value)
      entry
    (if value
	(make-instance 'n-struct
		       :name (real-name symbol)
		       :code (format nil "typedef TL_internal::~A result;~%"
				         (real-name value)))
	(make-instance 'n-struct
		       :name (real-name symbol)
		       :code ""))))

(defun make-quote (form)
  (let ((symbol (second form)))
    (typecase symbol
      (integer (make-integer symbol))
      (boolean (make-bool symbol))
      (t (set-symbol symbol)
	 (make-instance 'definition
			:name (format nil "TL_symbols::~A" (real-name symbol))
			:type 'type
			:access nil
			:body "")))))

(defun make-literal (literal)
  (let ((lit-name (real-name literal)))
    (make-instance 'definition
		   :name lit-name
		   :type 'type
		   :access 'public
		   :body "")))

(defun rename-tree (tree new-value predicate)
  (cond ((and (atom tree)
	      (funcall predicate tree))
	 new-value)
	((or (atom tree)
	     (eq (first tree)
		 'quote))
	 tree)
	(t (mapcar (lambda (elem)
		     (rename-tree elem new-value predicate)) tree))))

(defun rename-expr (old-name new-name expr)
  (rename-tree expr new-name (lambda (name)
			       (or (eql name old-name)
				   (and (typep name '(or string symbol))
					(typep old-name '(or string symbol))
					(string= (real-name old-name)
						 (real-name name)))))))

(defun make-let (form)
  (destructuring-bind (bindings &rest forms)
      (rest form)
    (let ((forms (reduce (lambda (forms binding)			  
			  (let ((new-name (to-string (gensym "let"))) ;;optimize it away if needed.
				(old-name (first binding))
				(value-name (if (typep (second binding) '(or string symbol))
						(real-name (second binding))
						(name (translate (second binding))))))
			    (setf *body* (append *body*
						 (list (make-instance 'definition
								      :name new-name
								      :type 'type
								      :access 'public
								      :body (format nil "~A" value-name)))))
			    (rename-expr old-name new-name forms)))
			bindings
			:initial-value forms)))
      (car (last (mapcar 'translate forms))))))

(defun make-thunk (name form arm)
  (let* (*body*
	 (dummy-name (to-string (gensym "dummy")))
	 (form (translate form))
	 (form-thunk (make-instance 'definition
				    :name name
				    :type 'template
				    :access 'public
				    :body (make-instance 't-class
							 :name name
							 :args (list (make-instance 'argument
										    :name dummy-name
										    :type 'typename))
							 :spec-args (list (if arm
									      "TL_core::True"
									      "TL_core::False")
									  dummy-name)
							 :code (append *body*
								       (list (make-instance 'definition
											    :name "result"
											    :type 'type
											    :access 'public
											    :body (format nil "~A" (name form)))))))))
    form-thunk))
	     
(defun make-if (form)
  (destructuring-bind (cond conseq &optional (altern nil))
      (rest form)
    (let* ((result-name (to-string (gensym "if")))
	   (arms-name (to-string (gensym "if_arms")))
	   (cond (translate cond))
	   (thunk (make-instance 'definition
				 :name arms-name
				 :type 'template
				 :access 'public
				 :body (make-instance 't-class
						      :name arms-name
						      :args (list (make-instance 'argument
										 :name "Cond"
										 :type 'typename)
								  (make-instance 'argument
										 :name (to-string (gensym "dummy"))
										 :type 'typename)))))
	   (conseq-thunk (make-thunk arms-name conseq t))
	   (altern-thunk (make-thunk arms-name altern nil))
	   (result (make-instance 'definition
				  :name result-name
				  :type 'type
				  :access 'public
				  :body (if *in-template*
					     (format nil "typename ~A<~A, dummyType>::result" arms-name (name cond))
					     (format nil "~A<~A, dummyType>::result" arms-name (name cond))))))
      (setf *body* (append *body* (list thunk conseq-thunk altern-thunk result)))
      result)))

(defun make-lambda (form)
  (destructuring-bind (args &rest expr)
      (rest form)
    (let* ((name (to-string (gensym "lambda")))
	   (new-args (mapcar (lambda (arg)
			       (cons (real-name (gensym (concatenate 'string "arg" (real-name arg))))
				     arg))
			     args))
	   (new-expr (reduce (lambda (expr names)
			       (rename-expr (cdr names) (car names) expr))
			     new-args
			     :initial-value expr))
	   (inner-defn (inner-make-defun name
					 (mapcar #'car new-args)
					 new-expr
					 :closure t))
	   (defn (make-instance 'definition :name name
				            :type 'struct
					    :access 'public
					    :body inner-defn)))
      (setf *body* (append *body* (list defn)))
      defn)))

(defun make-integer (int)
  (make-instance 'definition
		 :name (format nil "TL_core::BoxedInt<~A>" int)
		 :type 'type
		 :access 'public
		 :body ""))

(defun make-bool (bool)
  (make-instance 'definition
		 :name (if bool
			   "TL_core::True"
			   "TL_core::False")
		 :type 'type
		 :access 'public
		 :body ""))

(defun make-call (form &key (closure t))
  (destructuring-bind (fun &rest args)
      form
    (let* ((fun-name (if (symbolp fun)
			 (real-name fun)
			 (name (translate fun))))
	   (name (to-string (gensym "call")))
	   (args (mapcar #'translate args))
	   (defn (make-instance 'definition :name name
				            :type 'type
					    :access 'public
					    :body (with-output-to-string (out)
						    (if *in-template*
							(format out "typename ~A" (real-name fun-name))
							(format out "~A" (real-name fun-name)))
						    (when closure (if (and *in-template*
									   args)
								      (format out "::template call")
								      (format out "::call")))
						    (when args
						      (format out "< ~A" (real-name (name (first args))))
						      (dolist (arg (rest args))
							(format out ", ~A" (real-name (name arg))))
						      (format out ", TL_core::dummyType")
						      (format out " >"))
						    (format out "::result")))))
      (setf *body* (append *body* (list defn)))
      defn)))

(defun make-defun (form)
  (destructuring-bind (name args &rest expression)
      (rest form)
    (set-symbol name name)
    (inner-make-defun name args expression :closure t)))

(defun peak-char (stream)
  (let ((char (read-char stream nil nil)))
    (when char
      (unread-char char stream))
    char))

(defun break-string (string)
  (let (acc
	cur-string)
    (with-input-from-string (in string)
      (labels ((inner ()
		 (let ((char (read-char in nil nil)))
		   (when char
		     (if (and (eql char #\#)
			      (eql (peak-char in) #\{))
			 (progn
			   (push (coerce (nreverse cur-string) 'string)
				 acc)
			   (read-char in)
			   (setf acc (nconc (reverse (read-delimited-list #\} in))
					    acc))
			   (setf cur-string nil))
			 (push char cur-string))
		     (inner)))))
	(inner)
	(when cur-string
	  (unless (eql (first cur-string) #\Newline)
	    (push #\Newline cur-string))
	  (push (coerce (nreverse cur-string) 'string) acc))
	(nreverse acc)))))

(defun make-out (form)
  (destructuring-bind (type name options &rest data)
      (rest form)
    (destructuring-bind (&key (access 'public) (parent) (inherit-type))
	options
      (let* ((data (mapcan (lambda (datum)
			     (if (stringp datum)
				 (break-string datum)
				 (list datum)))
			   data))
	     (name (or name
		       (gensym "out")))
	     (body (with-output-to-string (out)
		     (mapcar (lambda (chunk)
			       (if (stringp chunk)
				   (format out "~A" chunk)
				   (format out "~A" (real-name (name (translate (tlmacroexpand chunk)))))))
			     data)))
	     (defn (make-instance 'definition
				  :name (real-name name)
				  :type type
				  :access access ;;always inside another class, be it toplevel or a user defined one
				  :body (ecase type
					  ((int bool raw) body)
					  ((type) body)
					  ((struct) (make-instance 'n-struct
								   :name (real-name name)
								   :parent parent
								   :inherit-type inherit-type
								   :code body))
					  ((class) (make-instance 'n-class
								  :name (real-name name)
								  :parent parent
								  :inherit-type inherit-type
								  :code body))))))
	(setf *body* (append *body* (list defn)))
	defn))))

(defun inner-make-defun (name args expressions &key closure)
  (let (*body*
	(*in-template* args)
	(*toplevel* nil))
    (declare (special *body* *in-template* *toplevel*))
    (let* ((result (car (last (mapcar 'translate expressions))))
	   (arguments (mapcar (lambda (argname)
				(if (consp argname)
				    (make-instance 'argument :name (real-name (first argname))
					                     :type 'typename
							     :default (second argname))
				    (make-instance 'argument :name (real-name argname)
						             :type 'typename)))
			      (append args (when closure
					      (list (to-string (gensym "dummy")))))))
	   (result-defn (make-instance 'definition :name "result"
				                   :type 'type
						   :access 'public
						   :body (format nil "~A" (name result))))
	   (inner (if args
		      (make-instance 't-class
				     :name (if closure
					       "call"
					       (real-name name))
				     :args arguments					  
				     :code (append *body* (list result-defn)))
		      (make-instance 'n-class
				     :name (if closure
					       "call"
					       (real-name name))
				     :code (append *body* (list result-defn))))))
      (if closure
	  (make-instance 'n-struct :name (real-name name)
			           :code (list (make-instance 'definition
					   	              :name ""
						              :type 'template
						              :access 'public
						              :body inner)))
	  inner))))

(defun make-define (form)
  (destructuring-bind (name expression)
      (rest form)
    (set-symbol name name)
    (inner-make-define name expression)))

(defun inner-make-define (name expression)
  (let* ((*toplevel* nil)
	 (calc-name (to-string (gensym "define")))
	 (calc (inner-make-defun calc-name '() expression :closure nil))
	 (result (make-instance 'definition
				:name (real-name name)
				:type 'type
				:access nil
				:body (format nil "~A::result" calc-name))))
    (declare (special *toplevel*))
    (list calc result)))

(defun public-definition (name &optional (args nil argsp))
  (with-output-to-string (out)
    (cond (args (progn
		  (format out "template < ")
		  (format out "typename ~A" (real-name (first args)))
		  (mapcar (lambda (arg)
			    (format out ", typename ~A" (real-name arg)))
			  (rest args))
		  (format out " >~%")
		  (format out "struct ~A~%" (real-name name))
		  (format out "{~%")
		  (format out "typedef TL_internal::~A::template call< " (real-name name))
		  (format out "~A" (real-name (first args)))
		  (mapcar (lambda (arg)
			    (format out ", ~A" (real-name arg)))
			  (rest args))
		  (format out ", TL_core::dummyType > eval;~%")
		  (format out "};~%")))
	  (argsp (format out "struct ~A { typedef TL_internal::~A::call eval; };~%" (real-name name) (real-name name)))
	  (t (format out "typedef TL_internal::~A ~A;~%" (real-name name) (real-name name)))))) ;;define

(defun compile-to-cpp (&rest forms)
  (let* (*to-declare*
	 *body*
	 (*symbols* *symbols*)
	 (*toplevel* t)
	 (definitions (mapcan (lambda (form)
				(let ((translation (translate form)))
				  (when (member (first form) '(defun define) :test #'equal)
				    (list (to-cpp translation))))) ;;otherwise not defn, shove in toplevel
			      forms)))
    (declare (special *to-declare* *body* *symbols* *toplevel*))
    (let ((*in-template* nil)
	  (definitions (append definitions
			       (when *body*
				 (list (to-cpp (make-instance 'n-class
							      :name "toplevel"
							      :code (append *body*
									    (list (make-instance 'definition
												 :name "result"
												 :type 'type
												 :access 'public
												 :body (format nil
													       "~A"
													       (name (car (last *body*))))))))))))))
      (declare (special *in-template*))
      (with-output-to-string (out)
	(format out "namespace TL_symbols~%{~%")
	(format out "using namespace TL_core_symbols;~%")
	(mapcar (lambda (entry)
		  (format out "~A~%" (declaration-string (define-symbol entry))))
		*symbols*)
	(format out "~%}~%~%")
	(format out "namespace TL_internal~%{~%")
	(format out "using namespace TL_core;~%")
	(when *to-declare*
	  (format out "~A~%~%" (declarations-string *to-declare*)))
	(mapcar (lambda (string)
		  (format out "~A~%" string))
		definitions)
	(format out "}~%~%")

	(format out "namespace TL_symbols~%{~%")
	(mapcar (lambda (entry)
		  (format out "~A~%" (to-cpp (define-symbol entry))))
		*symbols*)
	(format out "~%}~%~%")
	
	(format out "namespace TL~%{~%")
	(when *body*
	  (format out "struct toplevel { typedef TL_internal::toplevel::result result; };~%~%"))
	(mapcar (lambda (form)
		  (case (first form)
		    ((defun) (format out "~A~%" (public-definition (second form) (third form))))
		    ((define) (format out "~A~%" (public-definition (second form))))))
		forms)
	(format out "~%}~%")))))

(defvar *macroexpanders* ())

(defun add-macro (symbol function)
  (setf *macroexpanders* (acons symbol function
				(remove-if (lambda (entry)
					     (equal (car entry) symbol))
					   *macroexpanders*))))

(defun cond-macro (&rest branches)
  (if branches
        (destructuring-bind ((cond1 conseq1) &rest rest)
	    branches
	  (if rest
	      `(if ,cond1
		,conseq1
		(cond ,@rest))
	      `(if ,cond1
		,conseq1
		nil)))
	nil))

(add-macro 'cond 'cond-macro)

(defun TLmacroexpand-1 (form)
  "depth first search"
  (if (atom form)
      form
      (let ((expander (cdr (assoc (first form)
				  *macroexpanders*
				  :test #'equal))))
	(if expander
	    (values (apply expander (rest form))
		    T)
	    (let* (changep
		   (newform (mapcar (lambda (subform)
				      (multiple-value-bind (subform newchangep)
					  (TLmacroexpand-1 subform)
					(setf changep (or changep newchangep))
					subform))
				    form)))
	      (values newform changep))))))

(defun TLmacroexpand (form)
  (multiple-value-bind (form changep)
      (TLmacroexpand-1 form)
    (if changep
	(TLmacroexpand form)
	form)))

(defun TLcompile (&rest forms)
  (indent-code (apply 'compile-to-cpp (mapcar 'TLmacroexpand forms))))

(defmacro tl2cpp (&body forms)
  (apply 'TLcompile forms))