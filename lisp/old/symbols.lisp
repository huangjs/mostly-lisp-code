;;; list generic functions, nongeneric functions, variables in packages

;;; usage:

;;; list-functions						list all functions in the package
;;; list-generic-functions				list all generic functions
;;; list-nongeneric-functions			list all non-generic functions
;;; list-variables						list all variables in the package
;;; list-generic-function-on-class		list all generic functions that are defined on the class(s) (works only in sbcl because the error handle)

;;; predicates
(defun generic-function? (symbol)
  (and (fboundp symbol)
	   (typep (symbol-function symbol)
			  'generic-function)))

(defun nongeneric-function? (symbol)
  (and (fboundp symbol)
	   (not (typep (symbol-function symbol)
				   'generic-function))))

(defun function-defined-on-class? (function classes)
  (handler-case (find-method function '() (mapcar #'find-class classes) nil)
	;; this works only in sbcl
	;; for other implementations, change to other error class
	(sb-pcl::find-method-length-mismatch nil))) 

;;; filters
(defun filter-symbols (predicate &optional (package *package*))
  "Filter all the symbols in the specified package, including used packages
   fn, package(symbol or binding) => list of all symbols"
  (let ((symlst nil))
	(do-symbols (symbol package)
	  (when (funcall predicate symbol)
		(push symbol symlst)))
	symlst))

(defun filter-symbols-just-in-package (predicate &optional (package *package*))
  "Filter all the symbols in the specified package, but not including the used packages.
   fn, package(symbol or binding) => list of all symbols"
  (let ((symlst nil))
	(do-symbols (symbol package)
	  (when (and (eq package (symbol-package symbol)) ;in current package?
				 (funcall predicate symbol))
		(push symbol symlst)))
	symlst))

(defun filter-external-symbols (predicate &optional (package *package*))
  "Filter all the _external_ symbols in the specified package.
   fn, package(symbol or binding) => list of all external symbols"
  (let ((symlst nil))
	(do-external-symbols (symbol package)
	  (when (funcall predicate symbol)
		(push symbol symlst)))
	symlst))

(defun filter-all-symbols (predicate)
  ;;I think it searches all the external symbols in other packages and all the symbols in current package
  "Filter all (external) symbols (and all symbols in current package) in the lisp system.
   fn, package(symbol or binding) => list of all external symbols"
  (let ((symlst nil))
	(do-all-symbols (symbol)
	  (when (funcall predicate symbol)
		(push symbol symlst)))
	symlst))


;;; list-all-packages is already included in implementations
(defun list-functions (&optional (package *package*) (allow-used-packages nil))
  (let ((fn (if allow-used-packages
				#'filter-symbols
				#'filter-symbols-just-in-package))
		(p (find-package package)))
	(funcall fn #'fboundp p)))

(defun list-generic-functions (&optional (package *package*) (allow-used-packages nil))
  (let ((fn (if allow-used-packages
				#'filter-symbols
				#'filter-symbols-just-in-package))
		(p (find-package package)))
	(funcall fn #'generic-function? p)))

(defun list-nongeneric-functions (&optional (package *package*) (allow-used-packages nil))
  (let ((fn (if allow-used-packages
				#'filter-symbols
				#'filter-symbols-just-in-package))
		(p (find-package package)))
	(funcall fn #'nongeneric-function? p)))

(defun list-variables (&optional (package *package*) (allow-used-packages nil))
  (let ((fn (if allow-used-packages
				#'filter-symbols
				#'filter-symbols-just-in-package))
		(p (find-package package)))
	(funcall fn #'boundp p)))

(defun list-generic-functions-on-classes (classes)
  ;; limited only to generic functions
  "Find all generic functions that are defined on specific classes.
   Classes are a list of symbols of class names"
  (remove-if-not #'(lambda (sym)
					 (function-defined-on-class? (symbol-function sym) classes))
				 (list-generic-functions *package* t)))

