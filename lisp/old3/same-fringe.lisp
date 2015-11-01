;;; stack iterative protocol. :)
;;; unfortunately, very slow
;;; generator is still the fastest

(defun samefringe (t1 t2)
  (cond ((null t1) (null t2))
		(t ((lambda (t1 t2)
			  (and (eq (car t1) (car t2))
				   (samefringe (cdr t1) (cdr t2))))
			(fringe t1) (fringe t2)))))

(defun fringe (tree)
  (cond ((null tree) nil)
		((null (car tree)) (fringe (cdr tree)))
		((atom (car tree)) (list (car tree) (cdr tree)))
		(t (apply (lambda (&optional t1 t2)
					(cond (t1 (list t1 (cons t2 (cdr tree))))
						  (t (fringe (cdr tree)))))
				  (fringe (car tree))))))
