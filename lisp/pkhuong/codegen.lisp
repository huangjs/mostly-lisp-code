;; Issues: Memoise only c'tor or both c'tor and template builders?
;;         Simplify in the c'tors, only canonicalize in the builders...
;;
;; !!Add a transformation framework, and a pattern matching macro.
;; Otherwise, transformations (e.g. (+ (* ...) ...) -> BLAS) too ugly.

(cl:defpackage #:e
  (:export #:+ #:_ #:svar #:mvar #:*))

(cl:defpackage "CODEGEN"
  (:use #:cl))

(cl:in-package "CODEGEN")

;; data structures for the generator

(defclass codegen-data ()
  ((last-updt :accessor last-updt-of :initarg :last-updt)
   (props     :accessor props-of     :initarg :props)
   (code      :accessor code-of      :initarg :code)
   (freelist  :accessor freelist-of  :initarg :freelist)
   (to-build  :accessor to-build-of  :initarg :to-build))
  (:documentation
   "last-updt: id (eql) of last commit to `code'
    props    : name -> plist of properties
    code     : current cps'ed sexp.
    freelist : list of free resources
    tobuild  : list of objects that haven't been `build'ed yet."))

;;code: cps'ed (lambda (cont)
;;               `(... ,cont))
;; >>= does the binding.
;;
;; (lambda (cont)
;;    `(... ,cont))
;;
;; compose:
;;
;;
;; (compose (l1 l2)
;;    (lambda (cont)
;;       (funcall l1 (funcall l2 cont))))

(defun copy-hash-table (hash &optional (test 'eql))
  (let ((out (make-hash-table :test test)))
    (maphash (lambda (key value)
	       (setf (gethash key out)
		     value))
	     hash)
    out))

(defun codegen (original &key (last-updt (if original
					     (last-updt-of original)
					     (gensym "UPDT-ID")))
		              (props     (if original
					     (copy-hash-table (props-of     original))
					     (make-hash-table :test'equal)))
		              (code      (if original
					     (code-of      original)
					     #'identity))
		              (freelist  (and original
					      (copy-list (freelist-of  original))))
		              (to-build  (and original
					      (copy-list (to-build-of  original)))))
  (make-instance 'codegen-data :last-updt last-updt
		               :props     props
			       :code      code
			       :freelist  freelist
			       :to-build  to-build))

(defparameter *codegen-state* nil)

;;Functions to manipulate the state's codegen data

(defun get-to-build ()
  (to-build-of *codegen-state*))

(defun add-to-build (to-build)
  (push to-build (to-build-of *codegen-state*)))

(defun pop-to-build ()
  (pop (to-build-of *codegen-state*)))

(defun get-freelist ()
  (freelist-of *codegen-state*))

(defun free-p (obj)
  (and (member obj (freelist-of *codegen-state*))
       (not (in-to-build obj))
       (name-of obj)))

(defun free (obj)
  (unless (get-prop (name-of obj) :reused)
    (pushnew obj (freelist-of *codegen-state*)
	     :test #'eq)))

(defun unfree (obj)
  (set-prop (name-of obj) :reused t)
  (setf (freelist-of *codegen-state*)
	(delete obj (freelist-of *codegen-state*))))

(defun find-free (predicate)
  "If `nil' is free, prepare for badness."
  (let ((entry (find-if predicate (freelist-of *codegen-state*))))
    (when entry
      (unfree entry)
      entry)))

(defun props (name)
  "Returns the plist associated to name"
  (gethash name (props-of *codegen-state*)))

(defun get-prop (name prop &optional default)
  (getf (props name) prop default))

(defun clean-props (plist)
  (let ((props nil))
    (loop for (key value) on plist by #'cddr
	 append (unless (member key props)
		  (push key props)
		  (list key value)))))

(defun set-prop (name prop value)
  (setf (gethash name (props-of *codegen-state*))
	(clean-props (list* prop value (props name)))))

(defun compose-gen (g1 g2)
  (lambda (cont)
    (funcall g1 (funcall g2 cont))))

(defun add-code (generator)
  (setf (code-of *codegen-state*)
	(compose-gen (code-of *codegen-state*)
		     generator)))

(defgeneric %kill (obj name)
  (:documentation "Kill *self*"))

(defmethod %kill (obj name)
  nil)

(defun kill-from (name src)
  (let ((obj (get-prop name :object))
	(ref (get-prop name :references t))
	(def (get-prop name :defined)))
    (cond ((eq ref t)              nil)
	  ((null (remove src ref)) (let ((depends (get-prop name :depends-on)))
				     (mapcar (lambda (depend)
					       (kill-from depend name))
					     depends)
				     (when (and obj def)
				       (%kill obj name))
				     (set-prop name :references nil)))
	  (t                  (let ((refp (remove src ref)))
				(set-prop name :references refp))))))

(defun kill? (&rest names)
  (dolist (name names)
    (when (and (null (get-prop name :references t))
	       (not  (get-prop name :reused)))
      (dolist (depend (get-prop name :depends-on))
	(kill-from depend name))
      (let ((obj (get-prop name :object)))
	(when (and obj
		   (get-prop name :defined))
	  (%kill obj name))))))

(defun build-from (obj src)
  (set-prop src :depends-on (adjoin (name-of obj)
				    (get-prop src :depends-on)))
  (let ((refs (get-prop (name-of obj) :references))
	(deps (get-prop (name-of obj) :depends-on)))
    (mapcar (lambda (dependency)
	      (kill-from dependency (name-of obj)))
	    deps)
    (set-prop (name-of obj) :references (adjoin src refs))
    (%build obj)
    (name-of obj)))

#+nil (defun build-from-with-dep (obj src &rest deps)
	(unwind-protect
	     (progn
	       (mapcar #'add-to-build
		       deps)
	       (build-from obj src))
	  (mapcar (lambda (dep)
		    (declare (ignore dep))
		    (pop-to-build))
		  deps)))

(defgeneric %build (obj &key force)
  (:documentation "Defines the object (associate properties & thunk to its name)"))

(defmethod %build :around (obj &key force)
  (let ((name (name-of obj)))
    (set-prop name :object   obj)
    (mapcar (lambda (depend)
	      (kill-from depend name))
	    (get-prop name :depends-on))
    (set-prop name :depends-on nil)
    (cond (force   (call-next-method)
		   (setf (last-updt-of *codegen-state*) name)
		   (set-prop name :defined t)
		   (set-prop name :last-updt name))
	  ((eql (last-updt-of *codegen-state*)
		(get-prop name :last-updt))
	           name)
	  (t    (call-next-method)
		(set-prop name :last-updt (last-updt-of *codegen-state*))))
    name))

(defun force-from (name src &rest deps)
  "Forces the definition of the value associated to name"
  (unwind-protect
       (progn
	 (mapcar #'add-to-build
		 deps)
	 (set-prop src :depends-on (adjoin name
				    (get-prop src :depends-on)))
	 (let ((obj (get-prop name :object)))
	   (unless (or (null obj)
		       (get-prop name :defined))
	     (%build obj :force t))
	   (kill-from name src)
	   name))
    (mapcar (lambda (dep)
	      (declare (ignore dep))
	      (pop-to-build))
	    deps)
    (kill? name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defmemo (name (&rest args) &body body)
    "Defines a memoised function"
    (let ((var (intern (format nil "*MEMO-~A*" (symbol-name name))
		       (symbol-package name)))
	  (args (mapcar (lambda (arg)
			  (if (symbolp arg)
			      arg
			      (first arg)))
			(remove-if (lambda (arg)
				     (member arg lambda-list-keywords))
				   args)))
	  (_   (gensym "ARG-LIST"))
	  (values (gensym "VALUES"))
	  (foundp (gensym "FOUNDP")))
      `(progn
	 (defparameter ,var (make-hash-table :test 'equal))
	 (defun ,name ,args
	   (let ((,_ (list ,@args)))
	     (multiple-value-bind (,values ,foundp)
		 (gethash ,_ ,var)
	       (if ,foundp
		   (apply 'values ,values)
		   (apply 'values
			  (setf (gethash ,_ ,var)
				(multiple-value-list
				 (block ,name
				   ,@body)))))))))))

  (defmacro defcommmemo (name (&rest args) &body body)
    "Defines a memoised function (~commutative: (f a b ... z) = (f z ... b a))"
    (let ((var (intern (format nil "*MEMO-~A*" (symbol-name name))
		       (symbol-package name)))
	  (args (mapcar (lambda (arg)
			  (if (symbolp arg)
			      arg
			      (first arg)))
			(remove-if (lambda (arg)
				     (member arg lambda-list-keywords))
				   args)))
	  (_   (gensym "ARG-LIST"))
	  (values (gensym "VALUES"))
	  (foundp (gensym "FOUNDP")))
      `(progn
	 (defparameter ,var (make-hash-table :test 'equal))
	 (defun ,name ,args
	   (let ((,_ (list ,@args)))
	     (multiple-value-bind (,values ,foundp)
		 (gethash ,_ ,var)
	       (when ,foundp
		 (return-from ,name
		   (apply 'values ,values))))

	     (multiple-value-bind (,values ,foundp)
		 (gethash (reverse ,_) ,var)
	       (when ,foundp
		 (return-from ,name
		   (apply 'values ,values))))
	     (apply 'values
		    (setf (gethash ,_ ,var)
			  (multiple-value-list
			   (block ,name
			     ,@body))))))))))

(defparameter *dependencies* (make-hash-table :test 'eq))

(defgeneric get-deps (obj))

(defmethod get-deps :around (obj)
  (if (get-prop (name-of obj) :defined)
      (list obj)
      (multiple-value-bind (value foundp)
	  (gethash obj *dependencies*)
	(if foundp
	    value
	    (let* ((direct-deps (call-next-method))
		   (indirect    (mapcar 'get-deps direct-deps)))
	      (setf (gethash obj *dependencies*)
		    (adjoin obj (union direct-deps indirect))))))))

(defclass _ ()
  ((name :reader name-of :initarg :name :initform (gensym "NAME")))
  (:documentation "Parent class for placeholders"))

(defclass constant (_)
  ((value :reader value-of :initarg :value)))

(defmethod get-deps ((obj constant))
  nil)

(defmethod print-object ((obj constant) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (value-of obj))))

(defclass var (_)
  ((var :reader var-of :initarg :var)))

(defmethod get-deps ((obj var))
  nil)

(defmethod print-object ((obj var) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S" (var-of obj))))

(defclass op (_)
  ())

(defclass double ()
  ()
  (:documentation "Mixin: holds double data."))

(defclass scalar ()
  ()
  (:documentation "Mixin: scalar"))

(defclass dscalar (double scalar)
  ())

(defclass matrix ()
  ((height :reader height-of :initarg :height)
   (width  :reader width-of  :initarg :width))
  (:documentation "Mixin: matrix."))

(defclass dmatrix (double matrix)
  ()
  (:documentation "Mixin: matrix of doubles"))

(defmethod %kill ((obj dmatrix) name)
  (unless (in-to-build obj)
    (free obj)))

(defclass binop (_)
  ((left  :reader left-of  :initarg :left)
   (right :reader right-of :initarg :right)))

(defmethod get-deps ((obj binop))
  (list (left-of  obj)
	(right-of obj)))

(defmethod print-object ((obj binop) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S ~S ~S" (name-of obj) (left-of obj) (right-of obj))))

(defclass plus (binop)
  ())

(defclass mult (binop)
  ())

(defclass dsconstant (constant dscalar)
  ())

(defmemo dsconstant (value)
  (make-instance 'dsconstant
		 :value value))

(defclass dmconstant (constant dmatrix)
  ())

(defmethod %kill ((obj dmconstant) name)
  "Don't reuse constants"
  nil)

(defmemo dmconstant (value)
  (assert (and (listp value)
	       (let ((lrow (length (first value))))
		 (every (lambda (row)
			  (= (length row) lrow))
			(rest value)))))
  (make-instance 'dmconstant
		 :height (length value)
		 :width  (length (first value))
		 :value  value))

(defclass dmtemp (_ dmatrix)
  ())

(defun dmtemp (height width)
  (make-instance 'dmtemp
		 :height height
		 :width  width))

(defclass dsvar (var dscalar)
  ((min :reader min-of :initarg :min)
   (max :reader max-of :initarg :max)))

(defmemo dsvar (var &optional min max)
  (make-instance 'dsvar
		 :var var
		 :min min
		 :max max))

(defclass dmvar (var dmatrix)
  ())

(defmethod %kill ((obj dmvar) name)
  "Don't overwrite input variables"
  nil)

(defmemo dmvar (m n var)
  (make-instance 'dmvar :height m :width n :var var))

(defclass dsplus (plus dscalar)
  ())

(defcommmemo dsplus (a b)
  (assert (and (typep a 'dscalar)
	       (typep b 'dscalar))
	  (a b)
	  "Can't add ~A and ~A into a scalar double." a b)
  (make-instance 'dsplus :left  a
		         :right b))

(defclass dmplus (plus dmatrix)
  ())

(defun in-to-build (obj)
  (some (lambda (deps)
	  (member obj deps :test #'eq))
	(mapcar 'get-deps (to-build-of *codegen-state*))))

(defcommmemo dmplus (a b)
  (assert (and (typep a 'dmatrix)
	       (typep b 'dmatrix))
	  (a b)
	  "Can't add ~A and ~A into a matrix of doubles." a b)
  (assert (and (= (width-of a) (width-of b))
	       (= (height-of a) (height-of b)))
	  (a b)
	  "~A and ~A must be of equal dimensions." a b)
  (make-instance 'dmplus
		 :height (height-of a)
		 :width  (width-of a)
		 :left a
		 :right b))

(defclass dssmult (mult dscalar)
  ())

(defcommmemo dssmult (a b)
  (assert (and (typep a 'dscalar)
	       (typep b 'dscalar))
	  (a b)
	  "Can't multiply ~A and ~A into a scalar double." a b)
  (make-instance 'dssmult :left a :right b))

(defclass dsmmult (mult dmatrix)
  ())

(defcommmemo dsmmult (a b)
  (assert (or (and (typep a 'dscalar)
		   (typep b 'dmatrix))
	      (and (typep a 'dmatrix)
		   (typep b 'dscalar)))
	  (a b)
	  "Can't multiply ~A and ~A into a matrix of double." a b)
  (if (typep a 'dscalar)
      (make-instance 'dsmmult :left a :right b
		              :height (height-of b)
			      :width  (width-of  b))
      (make-instance 'dsmmult :left b :right a
		              :height (height-of a)
			      :width  (width-of  a))))

(defclass dmmmult (mult dmatrix)
  ())

(defmemo dmmmult (a b)
  (assert (and (typep a 'dmatrix)
	       (typep b 'dmatrix))
	  (a b)
	  "Can't multiply ~A and ~A into a matrix of double." a b)
  (assert (= (width-of a) (height-of b))
	  (a b)
	  "Width of ~A (~A) does not match height of ~A (~A)."
	  a (width-of  a)
	  b (height-of b))
  (make-instance 'dmmmult
		 :left a :right b
		 :height (height-of a)
		 :width  (width-of  b)))

(defun list-matrix-mult (a b)
  (let ((b (apply 'mapcar 'list b)))
    (assert (= (length (first a))
	       (length (first b))))
    (mapcar (lambda (row)
	      (mapcar (lambda (col)
			(reduce '+ (mapcar '* row col)))
		      b))
	    a)))

(defun list-matrix-scale (a b)
  (assert (or (and (numberp a)
		   (listp b))
	      (and (numberp b)
		   (listp a))))
  (if (numberp a)
      (mapcar (lambda (row)
		(mapcar (lambda (x)
			  (* x a))
			row))
	      b)
      (mapcar (lambda (row)
		(mapcar (lambda (x)
			  (* x b))
			row))
	      a)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defcomm (name (&rest args) &body body)
    "Commutative method"
    `(progn
       (defmethod ,name ,args
	 ,@body)
       (defmethod ,name ,(reverse args)
	 ,@body)))

  (defmacro memogf (name (&rest args)
		    &key (var-name (intern (format nil "*MEMO-~A*" (symbol-name name))
					   (symbol-package name)))
		    commutativep)
    "Marks a GF as memoised (around method specialised on T). Optionally ~ commutative."
    (labels ((to-symbol (x)
	       (if (consp x)
		   (to-symbol (first x))
		   x)))
      (let ((vars (mapcar #'to-symbol
			  (remove-if (lambda (entry)
				       (member entry lambda-list-keywords))
				     args))))
	`(progn
	   (defparameter ,var-name (make-hash-table :test 'equal))
	   (defmethod ,name :around ,args
	     (let ((args (list ,@vars)))
	       (multiple-value-bind (values foundp)
		   (gethash args ,var-name)
		 (when foundp
		   (return-from ,name (apply 'values values))))
	       ,@ (when commutativep
		    `((multiple-value-bind (values foundp)
			  (gethash (reverse args) ,var-name)
			(when foundp
			  (return-from ,name (apply 'values values))))))
	       (apply 'values
		      (setf (gethash args ,var-name)
			    (multiple-value-list (call-next-method))))))
	   ,@ (when commutativep
		`((defmethod ,name :around ,(reverse args)
		    (let ((args (list ,@vars)))
		      (multiple-value-bind (values foundp)
			  (gethash args ,var-name)
			(when foundp
			  (return-from ,name (apply 'values values))))
		      ,@ (when commutativep
			   `((multiple-value-bind (values foundp)
				 (gethash (reverse args) ,var-name)
			       (when foundp
				 (return-from ,name (apply 'values values))))))
		      (apply 'values
			     (setf (gethash args ,var-name)
				   (multiple-value-list (call-next-method)))))))))))))

(defmethod e:_ ((value number))
  (dsconstant value))

(defmethod e:_ ((value list))
  (dmconstant value))

(memogf e:_ (value))

(defmethod e:svar (name &optional min max)
  (dsvar name min max))

(memogf e:svar (name &optional min max))

(defmethod e:mvar (name m n)
  (dmvar m n name))

(memogf e:mvar (name m n))

(memogf e:+ (a b) :commutativep t)

(defcomm e:+ ((a scalar) (b matrix))
  (error "Tried to add scalar ~A to matrix ~A." a b))

;;this *really* wants a pattern matching DSL!

(defmethod e:+ ((a scalar) (b scalar))
  (dsplus a b))

(defmethod e:+ ((a matrix) (b matrix))
  (dmplus a b))

(defmethod e:+ :before ((a matrix) (b matrix))
  (assert (and (= (height-of a) (height-of b))
	       (= (width-of  a) (width-of  b)))
	  (a b)
	  "Can't add matrices of different shape (~A + ~A)"
	  a b))

(defmethod e:+ ((a dsconstant) (b dsconstant))
  (dsconstant (+ (value-of a)
		 (value-of b))))

(defun list-matrix-add (m1 m2)
  (mapcar (lambda (r1 r2)
	    (mapcar '+ r1 r2))
	  m1 m2))

(defmethod e:+ ((a dmconstant) (b dmconstant))
  (dmconstant (list-matrix-add (value-of a)
			       (value-of b))))

(defcomm e:+ ((a dsconstant) (b dscalar))
  (dsplus a b))

(defcomm e:+ ((a dmconstant) (b dmatrix))
  (dmplus a b))

(defcomm e:+ ((a dsconstant) (b dsplus))
  (if (typep (left-of b)
	     'dsconstant)
      (dsplus (dsconstant (+ (value-of a)
			     (value-of (left-of b))))
	      (right-of b))
      (dsplus a b)))

(defcomm e:+ ((a dmconstant) (b dmplus))
  (if (typep (left-of b)
	     'dmconstant)
      (dmplus (dmconstant (list-matrix-add (value-of a)
					   (value-of (left-of b))))
	      (right-of b))
      (dmplus a b)))

(defcomm e:+ ((a dsplus) (b dscalar))
  (if (typep (left-of a)
	     'dsconstant)
      (dsplus (left-of a)
	     (dsplus (right-of a)
		     b))
      (dsplus a b)))

(defcomm e:+ ((a dmplus) (b dmatrix))
  (if (typep (left-of a)
	     'dmconstant)
      (dmplus (left-of a)
	      (dmplus (right-of a)
		      b))
      (dmplus a b)))

(defmethod e:+ ((a dsplus) (b dsplus))
  (cond ((and (typep (left-of a)
		     'dsconstant)
	      (typep (left-of b)
		     'dsconstant)) (dsplus (dsconstant (+ (value-of (left-of a))
							  (value-of (left-of b))))
					   (dsplus (right-of a)
						   (right-of b))))
	((typep (left-of a) 'dsconstant) (dsplus (left-of a)
						 (dsplus (right-of a)
							 b)))
	((typep (left-of b) 'dsconstant) (dsplus (left-of b)
						 (dsplus a
							 (right-of b))))
	(t                               (dsplus a b))))

(defmethod e:+ ((a dmplus) (b dmplus))
  (cond ((and (typep (left-of a)
		     'dmconstant)
	      (typep (left-of b)
		     'dmconstant)) (dmplus (dmconstant (list-matrix-add (value-of (left-of a))
									(value-of (left-of b))))
					   (dmplus (right-of a)
						   (right-of b))))
	((typep (left-of a) 'dmconstant) (dmplus (left-of a)
						 (dmplus (right-of a)
							 b)))
	((typep (left-of b) 'dmconstant) (dmplus (left-of b)
						 (dmplus a
							 (right-of b))))
	(t                               (dmplus a b))))

(defgeneric e:* (a b))

(memogf e:* ((a scalar) (b scalar))
	:commutativep t)

(memogf e:* ((a scalar) (b matrix))
	:commutativep t)

(memogf e:* ((a matrix) (b matrix)))

(defmethod e:* ((a dscalar) (b dscalar))
  (dssmult a b))

(defmethod e:* ((a dsconstant) (b dsconstant))
  (dsconstant (* (value-of a)
		 (value-of b))))

(defcomm e:* ((a dsconstant) (b dscalar))
  (dssmult a b))

(defcomm e:* ((a dsconstant) (b dssmult))
  (if (typep (left-of a) 'dsconstant)
      (dssmult (dsconstant (* (value-of (left-of a))
			      (value-of (left-of b))))
	       (right-of b))
      (dssmult a b)))

(defcomm e:* ((a dssmult) (b dssmult))
  (cond ((and (typep (left-of a) 'dsconstant)
	      (typep (left-of b) 'dsconstant)) (dssmult (dsconstant (* (value-of (left-of a))
								       (value-of (left-of b))))
							(dssmult (right-of a)
								 (right-of b))))
	((typep (left-of a) 'dsconstant)       (dssmult (left-of a)
							(dssmult (right-of a)
								 b)))
	((typep (left-of b) 'dsconstant)       (dssmult (left-of b)
							(dssmult a
								 (right-of b))))
	(t                                     (dssmult a b))))

(defcomm e:* ((a dscalar) (b dmatrix))
  (dsmmult a b))

(defcomm e:* ((a dsconstant) (b dmatrix))
  (dsmmult a b))

(defcomm e:* ((a dsconstant) (b dmconstant))
  (dmconstant (list-matrix-scale (value-of a)
				 (value-of b))))

(defcomm e:* ((a dsconstant) (b dsmmult))
  (cond ((typep (left-of b) 'dsconstant)  (dsmmult (dsconstant (* (value-of a)
								  (value-of (left-of b))))
						   (right-of b)))
	((and (typep (left-of b) 'dssmult)
	      (typep (left-of (left-of b)) 'dsconstant))
	                                  (dsmmult (dssmult (dsconstant (* (value-of a)
									   (value-of
									    (left-of (left-of b)))))
							    (right-of (left-of b)))
						   (right-of b)))
	(t                                 (dsmmult a b))))

(defmethod e:* ((a dmatrix) (b dmatrix))
  (dmmmult a b))

(defmethod e:* ((a dmconstant) (b dmconstant))
  (dmconstant (list-matrix-mult (value-of a)
				(value-of b))))

(defmethod e:* ((a dmconstant) (b dsmmult))
  (if (typep (right-of b) 'dmconstant)
      (dsmmult (left-of b)
	       (dmconstant (list-matrix-mult (value-of a)
					     (value-of (right-of b)))))
      (dsmmult (left-of b)
	       (dmmmult a (right-of b)))))

(defmethod e:* ((a dsmmult) (b dmconstant))
  (if (typep (right-of a) 'dmconstant)
      (dsmmult (left-of a)
	       (dmconstant (list-matrix-mult (value-of (right-of a))
					     (value-of b))))
      (dsmmult (left-of a)
	       (dmmmult (right-of a) b))))

(defmethod e:* ((a dsmmult) (b dmatrix))
  (dsmmult (left-of a)
	   (dmmmult (right-of a)
		   b)))

(defmethod e:* ((a dmatrix) (b dsmmult))
  (dsmmult (left-of b)
	   (dmmmult a (right-of b))))

(defmethod e:* ((a dsmmult) (b dsmmult))
  (cond ((and (typep (left-of a) 'dsconstant)
	      (typep (left-of b) 'dsconstant)) (dsmmult (dsconstant (* (value-of (left-of a))
								       (value-of (left-of b))))
							(dmmmult (right-of a)
								 (right-of b))))
	((and (typep (left-of a) 'dsconstant)
	      (typep (left-of b) 'dssmult)
	      (typep (left-of (left-of b)) 'dsconstant))
	                                       (dsmmult (dssmult (dsconstant (* (value-of (left-of a))
										(value-of (left-of (left-of b)))))
								 (right-of (left-of b)))
							(dmmmult (right-of a) (right-of b))))
	((and (typep (left-of b) 'dsconstant)
	      (typep (left-of a) 'dssmult)
	      (typep (left-of (left-of a)) 'dsconstant))
	                                       (dsmmult (dssmult (dsconstant (* (value-of (left-of (left-of a)))
									       (value-of (left-of b))))
								 (right-of (left-of a)))
							(dmmmult (right-of a) (right-of b))))
	((typep (left-of a) 'dsconstant)  (dsmmult (dssmult (left-of a)
							    (left-of b))
						   (dmmmult (right-of a)
							    (right-of b))))
	((typep (left-of b) 'dsconstant)   (dsmmult (dssmult (left-of b)
							    (left-of a))
						   (dmmmult (right-of a)
							    (right-of b))))
	(t                                (dsmmult (dssmult (left-of a)
							    (left-of b))
						   (dmmmult (right-of a)
							    (right-of b))))))

(defun get-code (obj)
  (let* ((*codegen-state* (codegen *codegen-state*))
	 (name            (force-from (build-from obj :root)
				      :root)))
    (values (funcall (code-of *codegen-state*)
		     name)
	    *codegen-state*)))

(defun add-ref (name ref)
  (set-prop name
	    :references
	    (adjoin ref (get-prop name :references))))

(defmethod %build ((obj dsconstant) &key force)
  (with-slots (name value)
      obj
    (set-prop name :min value)
    (set-prop name :max value)
    (set-prop name :constant value)
    (when force
      (add-code (lambda (cont)
		  `(let ((,name ,(coerce value 'double-float)))
		     ,cont))))))

(defmethod %build ((obj dmconstant) &key force)
  (with-slots (name height width value)
      obj
    (set-prop name :constant value)
    (when force
      (add-code (lambda (cont)
		  `(let ((,name (make-array '(,height ,width) :element-type 'double-float
					    :initial-contents ',(mapcar (lambda (row)
									  (mapcar (lambda (elt)
										    (coerce elt 'double-float))
										  row))
									value))))
		     (declare (type (simple-array double-float (,height ,width))
				    ,name))
		     ,cont))))))

(defmethod %build ((obj var) &key force)
  (with-slots (name var)
      obj
    (when force
      (add-code (lambda (cont)
		  `(let ((,name ,var))
		     ,cont))))))

(defmethod %build ((obj dsvar) &key force)
  (with-slots (name var)
      obj
    (set-prop name :min (min-of obj))
    (set-prop name :max (max-of obj))
    (when force
      (add-code (lambda (cont)
		  `(let ((,name ,var))
		     (declare (type (double-float ,(if (min-of obj)
						       (coerce (min-of obj)
							       'double-float)
						       '*)
						  ,(if (max-of obj)
						       (coerce (max-of obj)
							       'double-float)
						       '*))
				    ,name))
		     ,cont))))))

(defmethod %build ((obj dmvar) &key force)
  (with-slots (name var height width)
      obj
    (when force
      (add-code (lambda (cont)
		  `(let ((,name ,var))
		     (declare (type (simple-array double-float (,height ,width))
				    ,name))
		     ,cont))))))

(defmethod %build ((obj dmtemp) &key force)
  (when force
    (with-slots (name height width)
	obj
      (add-code (lambda (cont)
		  `(let ((,name (make-array '(,height ,width) :element-type 'double-float)))
		     (declare (type (simple-array double-float (,height ,width))
				    ,name))
		     ,cont))))))

(defmethod %build ((obj dsplus) &key force)
  (with-slots (name left right)
      obj
    (let* ((name-left  (build-from left  name))
	   (name-right (build-from right name))
	   (ct-left    (get-prop name-left  :constant))
	   (ct-right   (get-prop name-right :constant))
	   (min-left   (get-prop name-left  :min))
	   (min-right  (get-prop name-right :min))
	   (max-left   (get-prop name-left  :max))
	   (max-right  (get-prop name-right :max))
	   (ct         (and ct-left ct-right
				      (+ ct-left ct-right)))
	   (min        (and min-left min-right
			    (+ min-left min-right)))
	   (max        (and max-left max-right
			     (+ max-left max-right)))
	   (form       (if ct
			   (coerce ct 'double-float)
			   `(+ , (if ct-left
				     (coerce ct-left 'double-float)
				     name-left)
				 , (if ct-right
				       (coerce ct-right 'double-float)
				       name-right)))))
      (when ct-left
	(kill-from name-left name))
      (when ct-right
	(kill-from name-right name))
      (set-prop name :min min)
      (set-prop name :max max)
      (set-prop name :constant ct)
      (when force
	(unless ct-left
	  (force-from name-left  name right))
	(unless ct-right
	  (force-from name-right name left))
	(add-code (lambda (cont)
		    `(let ((,name ,form))
		       (declare (type (double-float ,(if min
							 (coerce min 'double-float)
							 '*)
						    ,(if max
							 (coerce max 'double-float)
							 '*))
				      ,name))
		       ,cont)))))))

(defun recursive-coerce (obj type)
  (if (atom obj)
      (coerce obj type)
      (mapcar (lambda (obj)
		(recursive-coerce obj type))
	      obj)))

(defun find-free-dmatrix (height width)
  (find-free (lambda (entry)
	       (and (not (in-to-build entry))
		    (typep entry 'dmatrix)
		    (= (height-of entry) height)
		    (= (width-of  entry) width)))))

(defmethod %build ((obj dmplus) &key force)
  (with-slots (name left right height width)
      obj
    (let* ((name-left  (build-from left  name))
	   (name-right (build-from right name))
	   (ct-left    (get-prop name-left  :constant))
	   (ct-right   (get-prop name-right :constant))
	   (ct         (and ct-left ct-right
			    (list-matrix-add ct-left ct-right))))
      (set-prop name :constant ct)

      (when ct
	(kill-from name-left  name)
	(kill-from name-right name))
      
      (when force
	(unless ct
	  (force-from name-left  name right)
	  (force-from name-right name left))
	(let ((form (cond (ct `(make-array '(,height ,width)
					   :element-type 'double-float
					   :initial-contents ',(recursive-coerce ct 'double-float)))
			  ((free-p left)  (unfree left)
			   `(m+-into ,name-left ,name-right ,name-left))
			  ((free-p right) (unfree right)
			   `(m+-into ,name-left ,name-right ,name-right))
			  (t               	(let ((free (find-free-dmatrix height width)))
						  (if free
						      `(m+-into2 ,name-left ,name-right ,(name-of free))
						      `(m+ ,name-left ,name-right)))))))
	  (add-code (lambda (cont)
		      `(let ((,name ,form))
			 (declare (type (simple-array double-float (,height ,width))
					,name))
			 ,cont))))))))

(defmethod %build ((obj dssmult) &key force)
  (with-slots (name left right)
      obj
    (let* ((name-left  (build-from left  name))
	   (name-right (build-from right name))
	   (min-left   (get-prop name-left  :min))
	   (min-right  (get-prop name-right :min))
	   (max-left   (get-prop name-left  :max))
	   (max-right  (get-prop name-right :max))
	   (ct-left    (get-prop name-left  :constant))
	   (ct-right   (get-prop name-right :constant))
	   (extrema    (list (* max-left
				max-right)
			     (* min-left
				max-right)
			     (* max-left
				min-right)
			     (* min-left
				min-right)))
	   (min        (and min-left
			    min-right
			    (apply 'min extrema)))
	   (max        (and max-left
			    max-right
			    (apply 'max extrema)))
	   (ct        (and ct-left
			   ct-right
			   (* ct-left ct-right))))
      (set-prop name :min min)
      (set-prop name :max max)
      (set-prop name :constant ct)

      (when ct-left
	(kill-from name-left  name))
      (when ct-right
	(kill-from name-right name))
      
      (when force
	(unless ct-left
	  (force-from name-left  name))
	(unless ct-right
	  (force-from name-right name))

	(let ((form (cond (ct        (coerce ct 'double-float))
			  (ct-left  `(* ,(coerce ct-left 'double-float)
					,name-right))
			  (ct-right `(* ,name-left
					,(coerce ct-right 'double-float)))
			  (t        `(* ,name-left ,name-right)))))
	  (add-code (lambda (cont)
		      `(let ((,name ,form))
			 (declare (type (double-float ,(if min
							   (coerce min 'double-float)
							   '*)
						      ,(if max
							   (coerce max 'double-float)
							   '*))
					,name))
			 ,cont))))))))

(defmethod %build ((obj dsmmult) &key force)
  (with-slots (name left right height width)
      obj
    (let* ((name-left  (build-from left  name))
	   (name-right (build-from right name))
	   (ct-left    (get-prop name-left :constant))
	   (ct-right   (get-prop name-right :constant))
	   (ct         (and ct-left
			    ct-right
			    (list-matrix-scale left right))))
      (set-prop name :constant ct)

      (when ct-left
	(kill-from name-left name))
      
      (when force
	(unless ct
	  (unless ct-left
	    (force-from name-left  name right))
	  (force-from name-right name left))

	
	(let* ((name-scalar nil)
	       (form-scalar (let ((free (find-free-dmatrix 1 1)))
			      (if free
				  (setf name-scalar (name-of free))
				  (let* ((tmp      (dmtemp 1 1))
					 (name-tmp (build-from tmp name)))
				    (setf name-scalar name-tmp)
				    (force-from name-tmp name)))
			      (if ct-left
				  `(setf (aref ,name-scalar 0 0) ,(coerce ct-left 'double-float))
				  `(setf (aref ,name-scalar 0 0) ,name-left))))
	       (form (cond (ct       `(make-array `(,height ,width) :element-type 'double-float
						  :initial-content ',(recursive-coerce ct 'double-float)))
			   (ct-left  (if (free-p right)
					 (progn
					   (unfree right)
					   `(mscale-into ,name-scalar ,name-right ,name-right))
					 (let ((free (find-free-dmatrix height width)))
					   (if free
					       `(mscale-into2 ,name-scalar ,name-right ,free)
					       `(mscale ,name-scalar ,name-right)))))
			   (t        (if (free-p right)
					 (progn
					   (unfree right)
					   `(mscale-into ,name-scalar ,name-right ,name-right))
					 (let ((free (find-free-dmatrix height width)))
					   (if free
					       `(mscale-into2 ,name-left ,name-right ,free)
					       `(mscale ,name-scalar ,name-right))))))))

	  (kill-from name-scalar name)
	  (add-code (lambda (cont)
		      `(progn
			 ,form-scalar
			 (let ((,name ,form))
			   (declare (type (simple-array double-float (,height ,width))
					  ,name))
			   ,cont)))))))))

(defmethod %build ((obj dmmmult) &key force)
  (with-slots (name left right height width)
      obj
    (let* ((name-left  (build-from left  name))
	   (name-right (build-from right name))
	   (ct-left    (get-prop name-left  :constant))
	   (ct-right   (get-prop name-right :constant))
	   (ct         (and ct-left ct-right
			    (list-matrix-mult ct-left ct-right))))
      (set-prop name :constant ct)

      (when ct
	(kill-from name-left  name)
	(kill-from name-right name))
      
      (when force
	(unless ct
	  (force-from name-left  name right)
	  (force-from name-right name left))
	(let ((form (cond (ct `(make-array '(,height ,width)
					   :element-type 'double-float
					   :initial-contents ',(recursive-coerce ct 'double-float)))
			  ((free-p left)  (unfree left)
 			                 `(m*-into ,name-left ,name-right ,name-left))
			  ((free-p right) (unfree right)
			                 `(m*-into ,name-left ,name-right ,name-right))
			  (t               	(let ((free (find-free-dmatrix height width)))
						  (if free
						      `(m*-into2 ,name-left ,name-right ,(name-of free))
						      `(m* ,name-left ,name-right)))))))
	  (add-code (lambda (cont)
		      `(let ((,name ,form))
			 (declare (type (simple-array double-float (,height ,width))
					,name))
			 ,cont))))))))