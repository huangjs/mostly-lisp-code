(defun print-internal-definitions ()
  (iter (for names in (apropos-list "names" :sb-vm))
		(when (boundp names)
		  (terpri)
		  (print "***************************")
		  (format t "~&~%~A  contains:~%" names)
		  (map 'vector #L(when !1 (print !1)) (symbol-value names)))))
