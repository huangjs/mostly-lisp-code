(defun n-hammings (twos threes fives tail n out)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (= n 0)
	  out
	  (let* ((2l (* 2 (car twos)))
			 (3l (* 3 (car threes)))
			 (5l (* 5 (car fives)))
			 (next (min 2l 3l 5l)))
		(progn
		  (setf (cdr tail) (cons next ()))
		  (let ((out2 (if (= next 2l) (cdr twos) twos))
				(out3 (if (= next 3l) (cdr threes) threes))
				(out5 (if (= next 5l) (cdr fives) fives)))
			(n-hammings out2 out3 out5 (cdr tail) (1- n) out))))))


(defun n-hamming-nums (n)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((res (list 1)))
	(n-hammings res res res res n res)))

