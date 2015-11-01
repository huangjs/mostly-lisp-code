;;; from comp.lang.lisp

;;; functional iterator
(defun make-column-major-iterator (array)
  "e.g. (let ((a #3A(((1 2 3) (4 5 6)) ((7 8 9) (10 11 12)))))
            (loop with iter = (make-column-major-iterator a)
                  for (i p) = (multiple-value-list (funcall iter))
                  while p
                  do (print i))) "
  (let* ((dims    (coerce (array-dimensions array) 'vector))
         (indices (make-list (length dims) :initial-element 0))
         (done nil))
    (lambda ()
      (if done
          (values nil nil)
          (multiple-value-prog1 (values (apply (function aref) array indices) t)
	    (loop
	       :for i :from 0 :below (length dims)
	       :for index :on indices
	       :do (incf (car index))
	       :while (= (car index) (aref dims i))
	       :do (setf (car index) 0)
	       :finally (setf done (<= (length dims) i))))))))


;;; convert column-major number to row-major number
(defun column-major-accessor (array)
  (let* ((dims (array-dimensions array))
         (rdims (reverse dims))
         (rm-scales (reverse (maplist (lambda (l) (apply #'* (or (cdr l) '(1)))) dims)))
         (cm-scales (maplist (lambda (l) (apply #'* (or (cdr l) '(1))))
                             rdims)))
    (labels ((ref (i)
               (row-major-aref
                array
                (loop with source = i
		   with result = 0
		   for rm in rm-scales
		   for cm in cm-scales
		   do (multiple-value-bind (dim rest) (floor source cm)
			(setf result (+ result (* dim rm))
			      source rest))
		   finally (return result)))))
      #'ref)))

