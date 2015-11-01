(defun system-relative-pathname (system pathname &key name type)
  (let ((directory (pathname-directory pathname)))
    (when (eq (car directory) :absolute)
      (setf (car directory) :relative))
    (merge-pathnames
     (make-pathname :name (or name (pathname-name pathname))
                    :type (or type (pathname-type pathname))
                    :directory directory
					)
     (asdf-system-source-directory system))))

(defun asdf-system-source-directory (system-name)
  (make-pathname :name nil
				 :type nil
				 :defaults (asdf-system-source-file system-name)))

(defun asdf-system-source-file (system-name)
  (let ((system (asdf:find-system system-name)))
	(make-pathname 
	 :type "asd"
	 :name (asdf:component-name system)
	 :defaults (asdf:component-relative-pathname system))))
