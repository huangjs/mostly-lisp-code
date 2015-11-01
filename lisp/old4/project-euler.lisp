(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :hjs.math.number-theory))

(defun p1 ()
  (iter (for i below 1000)
	(when (or (zerop (mod i 3))
		  (zerop (mod i 5)))
	  (sum i))))


(defun p2 ()
  (let ((fibs (fibgen 1 2)))
    (apply #'+ (remove-if-not #'evenp (lazy-list:enumerate-all (hjs.data.lazy-sequence:take-when #L(< !1 1000000) fibs))))))


(defun p3 ()
  (car (last (factor-integer 317584931803 :pack nil))))




(defun digits-symmetric-p (digits)
  (iter (with len = (length digits))
	(for i from 0 below (floor len 2))
	(for j downfrom (1- len))
	(always (eql (aref digits i) (aref digits j)))))

(defun palindromic-p (num &key cache)
  (digits-symmetric-p (posint->digits num :into cache)))

(defun p4 ()
  (let ((cache (make-digits 6)))
    (iter outer (for i from 999 downto 100)
	  (iter (for j from 0 to (- 999 i))
		(for mul = (* (+ i j) (- i j)))
		(when (palindromic-p mul :cache cache)
		  (return-from p4 mul))))))


(defun p5 ()
  (apply #'lcm (lazy-list:enumerate-all (integers 1 20))))


(defun p6 ()
  (- (expt (sum-of-numbers 1 100) 2)
     (sum-of-square 1 100)))


(defun p7 ()
  (index->prime 10001))

