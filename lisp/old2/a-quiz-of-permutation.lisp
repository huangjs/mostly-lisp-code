(defun permutation (list)
  (if (null list)
      '(())
      [ (cons x ys) (x <- list) (ys <- (permutation (remove x list))) ]))

(defparameter *numbers* '(1 2 2 3 4 5))

(defun consecutive-p (pos1 pos2)
  (= (abs (- pos1 pos2))
     1))

(defun thousand-p (num list)
  (let ((thousand-position (- (length list) 4)))
    (eql (elt list thousand-position)
	 num)))

(defun curry (fn &rest args)
  (lambda (&rest more-args)
    (apply fn (append args more-args))))

(defun negate (fn)
  #'(lambda (&rest args)
      (not (apply fn args))))

;; from onLisp
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns 
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun  make-pipeline (&rest fns)
  (apply #'compose (reverse fns)))

(defun make-filter (predicate)
  (curry #'remove-if-not
	 predicate))

(defun print-digits (numbers)
  (loop for n in numbers
     do (princ n)
     finally (terpri)))


;;; after all the helper functions, here's my solution
(progn
  (let ((pipeline (make-pipeline #'permutation
				 #'(lambda (l)
				     (remove-duplicates l :test #'equal))
				 (make-filter (negate #'(lambda (numbers)
							  (consecutive-p (position 3 numbers)
									 (position 5 numbers)))))
				 (make-filter (negate (curry #'thousand-p 4))))))
    (mapc #'print-digits 
	  (funcall pipeline *numbers*))))
