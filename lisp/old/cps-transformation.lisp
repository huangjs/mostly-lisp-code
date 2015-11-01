;;; lexical binding!
(setf *cont* #'identity) 

(defmacro =lambda (parms &body body) 
  `#'(lambda (*cont* ,@parms) ,@body)) 

(defmacro =defun (name parms &body body) 
  (let ((f (intern (concatenate 'string "=" (symbol-name name))))) 
	`(progn 
	   (defmacro ,name ,parms 
		 `(,',f *cont* ,,@parms)) 
	   (defun ,f (*cont* ,@parms) ,@body)))) 

(defmacro =bind (parms expr &body body) 
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr)) 

(defmacro =values (&rest retvals) 
  `(funcall *cont* ,@retvals)) 

(defmacro =funcall (fn &rest args) 
  `(funcall ,fn *cont* ,@args)) 

(defmacro =apply (fn &rest args) 
  `(apply ,fn *cont* ,@args)) 

;;; FIXME buggy!!
;;; convention: continuation must be assigned as *cont*
(defmacro =flet (fdefinition &body body) ; 1-level only
  (destructuring-bind (fname fparams fbody) fdefinition
	(let ((f (intern (concatenate 'string "=" (symbol-name fname)))))
	  `(flet ((,f (*cont* ,@fparams)
				,fbody))
		 (macrolet ((,fname (,@fparams)
					  `(,',f *cont* ,,@fparams)))
		   ,@body)))))

