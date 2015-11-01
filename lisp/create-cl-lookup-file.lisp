(defparameter *packages-to-process*
  '((cl-ppcre "http://www.weitz.de/cl-ppcre/")
    (cl-fad "http://weitz.de/cl-fad/")
    (hunchentoot "http://weitz.de/hunchentoot/")
    (chunga "http://weitz.de/chunga/")
    (url-rewrite "http://weitz.de/url-rewrite/")
    (cl-who "http://weitz.de/cl-who/"))
  "List of packages and their documentation root urls.")

(defparameter *base-code*
  "(require 'cl-lookup)
(defvar cl-lookup-~a-root ~s)

(mapc #'(lambda (entry)
	  (destructuring-bind (name path) entry
	    (let ((symbol (intern (downcase name) cl-lookup-obarray)))
	      (if (boundp symbol)
		  (pushnew path (symbol-value symbol) :test #'equal)
		  (set symbol `(,path))))))
      '((~s (cl-lookup-~a-root \"\"))~%")

(defun create-lookup-file (&optional (packages *packages-to-process*))
  (dolist (p packages)
    (let* ((package (progn (quicklisp:quickload (first p)) (first p)))
	   (package-name (string-downcase (symbol-name package)))
	   (url (second p))
	   (output-file (format nil "cl-lookup-~a.el" package-name))
	   (symbols (sort (loop for sym being each external-symbol of package
                                collect (list (string-downcase (symbol-name sym))))
			  #'string-lessp :key #'car)))
      (with-open-file (s output-file :direction :output :if-exists :supersede)
	(format s *base-code* package-name url package-name package-name)
	(mapcar (lambda (sym)
		  (format s "~8t(\"~a:~a\" (cl-lookup-~a-root \"#~a\"))~%"
			  package-name
			  (string-downcase (car sym))
			  package-name
			  (string-downcase (car sym))))
		symbols)
	(format s "~8t))~%~%(provide 'cl-lookup-~a)" package-name)))))

;;;;
(defparameter *base-code-acl*
  "(require 'cl-lookup)
\(defvar cl-lookup-acl-operators-~a-root \"~a\")
\(defvar cl-lookup-acl-variables-~a-root \"~a\")
\(defvar cl-lookup-acl-classes-~a-root \"~a\")

\(mapc #'(lambda (entry)
	  (destructuring-bind (name path) entry
	    (let ((symbol (intern (downcase name) cl-lookup-obarray)))
	      (if (boundp symbol)
		  (pushnew path (symbol-value symbol) :test #'equal)
		  (set symbol `(,path))))))
      '(~%")

(defun acl-princ-sym (symbol &optional no-sub)
  (let ((name (string-downcase (symbol-name symbol))))
    (if (not (char= #\* (char name 0)))
        name
        (concatenate 'string
                     (if no-sub ""  "s_")
                     (subseq name 1 (1- (length name)))
                     (if no-sub ""  "_s")))))

(defun acl-operator-p (symbol)
  (fboundp symbol))

(defun acl-symbol-kind (symbol)
  (cond ((fboundp symbol)
         :function)
        ((ignore-errors (find-class symbol))
         :class)
        (t
         :variable)))

(defun create-acl-lookup-file ()
  (labels ((doit (package)
             (let* ((package-name (string-downcase (symbol-name package))) 
                    (output-file (format nil "cl-lookup-acl-~a.el" package-name))
                    (symbols (sort (loop for sym being each external-symbol of package
                                         collect sym)
                                   #'string-lessp
                                   :key #'symbol-name)))
               (with-open-file (s output-file :direction :output :if-exists :supersede)
                 (format s *base-code-acl*
                         package-name
                         (format nil "http://www.franz.com/support/documentation/8.2/doc/operators/~a/" package-name)
                         package-name
                         (format nil "http://www.franz.com/support/documentation/8.2/doc/variables/~a/" package-name)
                         package-name
                         (format nil "http://www.franz.com/support/documentation/8.2/doc/classes/~a/" package-name))
                 (mapcar (lambda (sym)
                           (format s "~8t(\"~a:~a\" (cl-lookup-acl-~a-~a-root \"~a.htm\"))~%"
                                   package-name
                                   (string-downcase (symbol-name sym))
                                   (case (acl-symbol-kind sym)
                                     (:class "classes")
                                     (:function "operators")
                                     (otherwise "variables"))
                                   package-name
                                   (acl-princ-sym sym)))
                         symbols)
                 (format s "~8t))~%~%(provide 'cl-lookup-acl-~a)" package-name)))))
    (doit 'excl)
    (doit 'socket)
    (doit 'system)
    (doit 'compiler)
    (doit 'mp)
    (doit 'dbi)))

(defun create-acl-lookup-file-aserve ()
  (labels ((doit (package root-url)
             (let* ((package-name (string-downcase (symbol-name package)))
                    (output-file (format nil "cl-lookup-acl-~a.el" package-name))
                    (symbols (sort (loop for sym being each external-symbol of package
                                         collect sym)
                                   #'string-lessp
                                   :key #'symbol-name)))
               (with-open-file (s output-file :direction :output :if-exists :supersede)
                 (format s *base-code-acl*
                         package-name root-url
                         package-name root-url
                         package-name root-url)
                 (mapcar (lambda (sym)
                           (format s "~8t(\"~a:~a\" (cl-lookup-acl-~a-~a-root \"#~a-~a\"))~%"
                                   package-name
                                   (string-downcase (symbol-name sym))
                                   (if (acl-operator-p sym)
                                       "operators"
                                       "variables")
                                   package-name
                                   (if (fboundp sym)
                                       "f"
                                       "v")
                                   (acl-princ-sym sym t)))
                         symbols)
                 (format s "~8t))~%~%(provide 'cl-lookup-acl-~a)" package-name)))))
    (doit 'net.aserve "http://www.franz.com/support/documentation/8.2/doc/aserve/aserve.html")
    (doit 'net.aserve.client "http://www.franz.com/support/documentation/8.2/doc/aserve/aserve.html")))


;;;;

