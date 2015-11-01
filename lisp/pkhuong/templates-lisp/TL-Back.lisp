(cl:in-package #:template-lisp)

(defvar *to-declare* ())
(defvar *to-declare-pub* ()
  "List of the public templates/types/functions to declare")
(defvar *to-declare-priv* ())
(defvar *to-declare-prot* ())
(defvar *to-prepend* ())

(defgeneric to-cpp (form &key access)
  (:documentation "prints a C++ objects to string (returns the string and adds it in to-declare)"))

(defmethod to-cpp ((forms list) &key access)
  (with-output-to-string (out)
    (mapcar #'(lambda (form)
		(format out "~A~%" (to-cpp form :access access)))
	    forms)))

(defun to-string (name &key pretty)
  (if (symbolp name)
      (if pretty
	  (capitalize (string-downcase (dash-to-underscore (symbol-name name))))
	  (string-downcase (symbol-name name)))
      name))

(defun template-arg-string (arg)
  (if (default arg)
      (format nil "~A ~A = ~A" (to-string (arg-type arg))
	                       (to-string (name arg))
			       (to-string (default arg)))
      (format nil "~A ~A" (to-string (arg-type arg))
	                  (to-string (name arg)))))

(defmethod to-cpp ((definition string) &key access)
  (declare (ignore access))
  definition)

(defmethod to-cpp ((definition definition) &key access)
  (declare (ignore access))
  (with-output-to-string (out)
    (when (access definition)
      (format out "~A: " (to-string (access definition))))
    (ecase (def-type definition)
      ((template struct class)
       (format out "~%~A" (to-cpp (body definition) :access (access definition))))
      ((type) (format out "typedef ~A ~A;" (body definition)
		                           (to-string (name definition))))
      ((int)  (format out "static const int ~A = ~A;" (to-string (name definition))
						       (body definition)))
      ((bool) (format out "static const bool ~A = ~A;" (to-string (name definition))
						       (body definition)))
      ((raw)  (format out "~A" (body definition))))))

(defgeneric declaration-string (declaration))

(defmethod declaration-string ((declaration t))
  "")

(defmethod declaration-string ((declaration n-class))
  (format nil "class ~A;" (name declaration)))

(defmethod declaration-string ((declaration n-struct))
  (format nil "struct ~A;" (name declaration)))

(defmethod declaration-string ((declaration t-class))
  (with-output-to-string (out)
    (if (args declaration)
	(let ((args (args declaration)))
	  (format out "template < ~A" (to-string (arg-type (first args))))
	  (dolist (arg (rest args))
	    (format out ", ~A" (to-string (arg-type arg))))
	  (format out " > "))
	(format out "template <> "))
    (format out "class ~A;" (name declaration))))

(defmethod declaration-string ((declaration t-struct))
  (with-output-to-string (out)
    (if (args declaration)
	(let ((args (args declaration)))
	  (format out "template < ~A" (to-string (arg-type (first args))))
	  (dolist (arg (rest args))
	    (format out ", ~A" (to-string (arg-type arg))))
	  (format out " > "))
	(format out "template <> "))
    (format out "struct ~A;" (name declaration))))

(defmethod declaration-string ((declaration t-function))
  (with-output-to-string (out)
    (format out "~A ~A (" (return-type declaration) (name declaration))
    (let ((args (function-args declaration)))
      (when args
	(format out "~A" (to-string (arg-type (first args))))
	(dolist (arg (rest args))
	  (format out ", ~A" (to-string (arg-type arg))))))
    (format out ");")))

(defun declarations-string (declarations)
  (with-output-to-string (out)
    (dolist (declaration declarations)
      (format out "~A~%" (declaration-string declaration)))))

(defmethod to-cpp ((class t-type) &key access)
  (when (null (spec-args class))
    (case access
      ((public) (pushnew class *to-declare-pub* :key #'name :test #'string=))
      ((private) (pushnew class *to-declare-priv* :key #'name :test #'string=))
      ((protected) (pushnew class *to-declare-prot* :key #'name :test #'string=))
      (otherwise (pushnew class *to-declare* :key #'name :test #'string=))))
  (with-output-to-string (out)
    (if (args class)
	(progn
	  (format out "template < ~A" (template-arg-string (first (args class))))
	  (dolist (arg (rest (args class)))
	    (format out ", ~A" (template-arg-string arg)))
	  (format out " >~%"))
	(format out "template <>~%"))
    (etypecase class
      (t-class (format out "class ~A " (name class)))
      (t-struct (format out "struct ~A " (name class))))
    (when (spec-args class)
      (format out "< ~A" (first (spec-args class)))
      (dolist (spec-arg (rest (spec-args class)))
	(format out ", ~A" spec-arg))
      (format out " > "))
    (when (parent class)
      (format out ": ~A ~A" (to-string (inherit-type class)) (to-string (parent class))))
    (format out "~%{~%")
    (if (listp (code class))
	(let* (*to-declare-pub* *to-declare-priv* *to-declare-prot* *to-prepend*
	       (body (with-output-to-string (body)
		       (dolist (definition (code class))
			(format body "~A~%" (to-cpp definition))))))
	  (declare (special *to-declare-pub* *to-declare-priv* *to-declare-prot*))
	  (when *to-declare-pub*
	    (format out "public:~%")
	    (format out "~A~%" (declarations-string *to-declare-pub*)))
	  (when *to-declare-priv*
	    (format out "private:~%")
	    (format out "~A~%" (declaration-string *to-declare-priv*)))
	  (when *to-declare-prot*
	    (format out "protected:~%")
	    (format out "~A~%" (declaration-string *to-declare-prot*)))
	  (when (or *to-declare-pub* *to-declare-priv* *to-declare-prot*)
	    (format out "~%"))
	  (when *to-prepend*
	    (dolist (line *to-prepend*)
	      (format out "~A~%" line))
	    (format out "~%"))
	  (format out "~A" body))
	(format out "~A" (code class)))
    (format out "};~%")))

(defmethod to-cpp ((class n-type) &key access)
  (case access
    ((public) (pushnew class *to-declare-pub* :key #'name :test #'string=))
    ((private) (pushnew class *to-declare-priv* :key #'name :test #'string=))
    ((protected) (pushnew class *to-declare-prot* :key #'name :test #'string=))
    (otherwise (pushnew class *to-declare* :key #'name :test #'string=)))
  (with-output-to-string (out)
    (etypecase class
      (n-class (format out "class ~A " (name class)))
      (n-struct (format out "struct ~A " (name class))))
    (when (parent class)
      (format out ": ~A ~A" (to-string (inherit-type class)) (to-string (parent class))))
    (format out "~%{~%")
    (if (listp (code class))
	(let* (*to-declare-pub* *to-declare-priv* *to-declare-prot* *to-prepend*
	       (body (with-output-to-string (body)
		       (dolist (definition (code class))
			(format body "~A~%" (to-cpp definition))))))
	  (declare (special *to-declare-pub* *to-declare-priv* *to-declare-prot*))
	  (when *to-declare-pub*
	    (format out "public:~%")
	    (format out "~A~%" (declarations-string *to-declare-pub*)))
	  (when *to-declare-priv*
	    (format out "private:~%")
	    (format out "~A~%" (declaration-string *to-declare-priv*)))
	  (when *to-declare-prot*
	    (format out "protected:~%")
	    (format out "~A~%" (declaration-string *to-declare-prot*)))
	  (when (or *to-declare-pub* *to-declare-priv* *to-declare-prot*)
	    (format out "~%"))
	  (when *to-prepend*
	    (dolist (line *to-prepend*)
	      (format out "~A~%" line))
	    (format out "~%"))
	  (format out "~A" body))
	(format out "~A~%" (code class)))
    (format out "};~%")))

(defun trim (string &key (from-end nil))
  (let ((pos (position-if (lambda (char)
			    (not (member char '(#\Space #\Tab #\NewLine #\Page #\LineFeed #\Return))))
			  string
			  :from-end from-end)))
    (if pos
	(if from-end
	    (subseq string 0 (1+ pos))
	    (subseq string pos))
	string)))

(defun trim-lines (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (labels ((trim-line ()
		 (let ((line (read-line in nil nil)))
		   (when line
		     (format out "~A~%" (trim (trim line) :from-end t))
		     (trim-line)))))
	(trim-line)))))

(defvar *indent-level* 0)

(defun access-modifierp (line)
  (let ((line (string-downcase line)))
    (some (lambda (substr)
	    (string= substr
		     (subseq line 0 (min (length substr)
					 (length line)))))
	  '("public:" "private:" "protected:"))))

(defun count-leading (needle line &key invert)
  (let ((end-of-lead (or (position-if (lambda (char)
				    (not (member char (list needle #\Space #\Tab #\Newline
							    #\Page #\LineFeed #\Return))))
				  line)
			 (1- (length line)))))
    (if (not invert)
	(count needle line :end (1+ end-of-lead))
	(count needle line :start (1+ end-of-lead)))))

(defun indent-code (code)
  (with-output-to-string (out)
    (with-input-from-string (in code)
      (labels ((inner ()
		 (let ((line (trim (trim (read-line in nil nil)) :from-end t)))
		   (when line
		     (let* ((*indent-level* (- *indent-level*
					       (count-leading #\} line)))
			    (indent (make-array (max 0
						     (* 8 (if (access-modifierp line)
							      (1- *indent-level*)
							      *indent-level*)))
						:element-type 'character
						:initial-element #\Space))
			    (*indent-level* (+ *indent-level*
					       (count #\{ line)
					       (- (count-leading #\} line :invert t)))))
		       (declare (special *indent-level*))
		       (format out "~A~A~%" indent line)
		       (inner))))))
	(inner)))))