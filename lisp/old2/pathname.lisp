(eval-when (:load-toplevel :compile-toplevel :execute)
  (let ((directory (pathname-directory *load-truename*)))
	(print (setf (logical-pathname-translations "lisp")
				 `((#-gcl "**;*.*.*" #+gcl "**;*.*" 
						  ,(make-pathname :directory `(,@directory :wild-inferiors)
										  :name :wild :type :wild :version :wild)))))
	(print *load-truename*)
	(print (translate-logical-pathname "lisp:femlisp;test.c"))))

(print #.(translate-logical-pathname #p"lisp:src;basic;"))

(print "*****************")

(print (merge-pathnames
		"runes/"
		(make-pathname :name nil :type nil :defaults *load-truename*)))
