(defun list-all-VOPs ()
  (let ((result '()))
	(maphash
	 (lambda (vop-name foo)
	   (declare (ignore foo))
	   (push vop-name result))
	 sb-c::*backend-parsed-vops*)
	(nreverse result)))

