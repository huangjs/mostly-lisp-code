(defparameter list '(13 4 (5 ((7 (8 8) 1) 3)) 5 7 (8 (5) 4 (3 (65) 78)) 8))

(defun flatten (input &optional accumulator)
  (cond ((null input) accumulator)
        ((atom input) (cons input accumulator))
        (t (flatten (first input)
                    (flatten (rest input) accumulator)))))


(defun test ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((list '(13 4 (5 ((7 (8 8) 1) 3)) 5 7 (8 (5) 4 (3 (65) 78)) 8)))
	(declare (dynamic-extent list))
	(repeat 500000
	  (flatten list))))
