#+sbcl(in-package "SB-ALIEN")
#+cmu(in-package "SYSTEM")

(defun absolute-namestring-p (namestring)
  (char= (char namestring 0) #\/))

(defun namestring-filename (namestring)
  (subseq namestring (1+ (position #\/ namestring :from-end t))))

#+sbcl
(defun try-reopen-shared-object (obj)
  (declare (type shared-object obj))
  (or
   (ignore-errors (dlopen-or-lose obj))
   (when (absolute-namestring-p (shared-object-file obj))
     (let ((namestring (shared-object-file obj)))
       (setf (shared-object-file obj) (namestring-filename namestring))
       (handler-case (dlopen-or-lose obj)
	 (simple-error (c) (setf (shared-object-file obj) namestring) nil))))
   (tagbody :dlopen 
      (restart-case (dlopen-or-lose obj)
	(continue ()
	  :report "Skip this shared object and continue."
	  (setf (shared-object-sap obj) nil))
	(retry ()
	  :report "Retry loading this shared object."
	  (go :dlopen))
	(load-other ()
	  :report "Specify an alternate shared object file to load."
	  (setf (shared-object-file obj)
		(tagbody :query
		   (format *query-io* "~&Enter pathname (evaluated):~%")
		   (force-output *query-io*)
		   (let ((pathname (ignore-errors (pathname (read *query-io*)))))
		     (unless (pathnamep pathname)
		       (format *query-io* "~&Error: invalid pathname.~%")
		       (go :query))
		     (unix-namestring pathname))))))))
  obj)

#+cmu
(defun try-dlopen (file)
  (dlerror)
  (let ((sap (dlopen (namestring (convert-object-file-path file))
		     (logior rtld-now rtld-global))))
    (unless (zerop (sap-int sap))
      (format t "Reloaded library ~S~%" file)
      (force-output)
      sap)))

#+cmu
(defun reinitialize-global-table ()
  (loop for lib-entry in (reverse *global-table*)
     for (sap . lib-path) = lib-entry
     as restarted = nil
     when lib-path
     do
     (loop
	(restart-case 
	    (let ((new-sap (or
			    (try-dlopen lib-path)
			    (when (and 
				   (not restarted)
				   (absolute-namestring-p lib-path))
			      (let* ((filename (namestring-filename lib-path))
				     (sap (try-dlopen filename)))
				(when sap
				  (setf (cdr lib-entry) filename)
				  sap))))))
	      (unless new-sap
		(error "Couldn't open library ~S: ~S" lib-path (dlerror)))
	      (setf (car lib-entry) (or new-sap (int-sap 0)))
	      (return))
	  (continue ()
	    :report "Ignore library and continue"
	    (return))
	  (try-again ()
	    :report "Try reloading again"
	    )
	  (new-library ()
	    :report "Choose new library path"
	    (format *query-io* "Enter new library path: ")
	    (setf lib-path (read))))
	(setq restarted t)))
  (alien:alien-funcall (alien:extern-alien "os_resolve_data_linkage"
                                           (alien:function c-call:void))))
