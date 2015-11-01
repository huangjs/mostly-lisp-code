(update (n)
		(when (= n 81) 
		  (restart-case 
			  (signal 'solution-found :solution soln)
			(keep-looking () 
			  (return-from update nil))))
		(if (nth-value-set-p n)
			(update (1+ n))
			(over-possible-digits (d n)
								  (set-digit n d)
								  (update (1+ n))
								  (clear-digit n))))


(define-condition solution-found (condition)
  ((solution :initarg :solution :reader solution)))


(defun find-first-solution (puzzle)
  (handler-case (solve2 puzzle)
    (solution-found (x) (solution x))))

(defun multiple-solutions-p (puzzle)
  (let ((found 0))
    (handler-bind  ((solution-found
					 #'(lambda (c) (declare (ignore c))
							   (when (> (incf found) 1)
								 (return-from multiple-solutions-p t))
							   (invoke-restart 'keep-looking))))
      (solve2 puzzle))))

(defun find-all-solutions (puzzle)
  (let ((solns `()))
    (handler-bind  
        ((solution-found
          #'(lambda (c)
              (push (make-array 81 :initial-contents (solution c)) solns)
			  (invoke-restart 'keep-looking))))
      (solve2 puzzle))
    solns))


(defun index->subs (n)
  (multiple-value-list (floor n 9)))
