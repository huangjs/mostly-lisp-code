(in-package "SB-BIGNUM")

(declaim (optimize (speed 3)))

;;;; Multiplication.

;;; **********************************************************************
;;;
;;; This Karatsuba implementation was written by Raymond Toy and
;;; Douglas T. Crosher and has been placed in the Public domain, and
;;; is provided 'as is'.
;;;
;;; **********************************************************************
;;;
;;; Karatsuba multiplication.
;;; References:  Knuth, Seminumerical Algorithms.
;;;

;;; Based on an implementation by Raymond Toy, who's explanation follows:
;;;
;;; Let U = (b^n)*U1 + U0 and V = (b^n)*V1 + V0 be two positive
;;; integers consisting of b^(2n) base-b digits.  Typically, we use
;;; either base 2 digits or base 2^32 digits.
;;;
;;; The product U*V can be written
;;;
;;;    U*V = (b^(2n))*U1*V1 + (b^n)(U0*V1 + U1*V0) + U0*V0
;;;
;;; However, the middle term can be written as
;;;
;;;    U0*V1 + U1*V0 = U0*V0 + U1*V1 - (U0 - U1)*(V0 - V1)
;;;
;;; Thus,
;;;
;;;    U*V = (b^(2n))*U1+V1 + (b^n)*[U1*V1 + U0*V0 - (U0 - U1)*(V0 - V1)]
;;;           U0*V0
;;;
;;; Note that now we require only three multiplies: U1*V1, U0*V0, and
;;; (U0 - U1)*(V0 - V1).  This should speed up multiplication
;;; considerably.
;;;
;;; For implemenation, we only want to work with positive numbers.
;;; Since U0-U1 or V0-V1 may be negative, we need to account for this.
;;; To do so, we compute
;;;
;;;    U1*V1 + U0*V0 - Su*Sv*|U0 - U1|*|V0 - V1|
;;;
;;; where Su = sign (U0 - U1) and Sv = sign (V0 - V1).
;;;
;;; Note that some authors use the expression
;;;
;;;    U0*V1 + U1*V0 = (U0 + U1)*(V0 + V1) - U0*V0 - U1*V1
;;;
;;; We don't because if U0 and U1 each contain b^n digits, U0 + U1 can
;;; overflow to b^n + 1 digits.  This complicates the multiplication quite a
;;; bit, so we use |U0 - U1|.  This is guaranteed not to overflow since U0 and
;;; U1 are both positive with b^n digits.
;;;


;;; karatsuba-internal  --  Internal.
;;;
;;; Main routine for Karatsuba multiplication.
;;; By Douglas Crosher.
;;;
;;; The result is placed in the result bignum starting at position RET, and is
;;; of size 2xD words. An extra D words are used during the calculations.
;;;
;;; The calculations are packed into this working space as follows:
;;;
;;; ------------------------------------------------------------------
;;; |    |   D/2   |   D/2   |  D/2    |   D/2   |   D/2   |  D/2    |
;;; |----+---------+---------+---------+---------+---------+---------|
;;; | A. | |u1-u0| | |v1-v0| |    x    |       |u1-u0|*|v1-v0|       |
;;; | B. |           u0 * v0           |       |u1-u0|*|v1-v0|       |
;;; | C. | u0 * v0 - S|u1-u0|*|v1-v0|  |             x               |
;;; | D. | u0 * v0 - S|u1-u0|*|v1-v0|  |          v1 * v0            |
;;; | E. | u0 * v0 - S|u1-u0|*|v1-v0| + v1 * v0  |    x    |    x    |
;;; ------------------------------------------------------------------
;;;
;;; A. Calculate |u1-u0| place the result in the first D/2 words, and
;;;    calculate |v1-v0| placing the result the second D/2 words. Then
;;;    calculate |u1-u0|*|v1-v0| using the last 3 D/2 words for the result and
;;;    subcalculate scratch area.
;;;
;;; B. Calculate the product u0*v0, using the first 3 D/2 words for the result
;;;    and subcalculate scratch area.
;;;
;;; C. Sum the results of first two stages into the first 3 D/2 words.
;;;    The calculation is (u0*v0) + ((u0*v0) - S|u1-u0|*|v1-v0|) *2^(d/2).
;;;
;;; D. Calculate the product u1*v1, using the last 3 D/2 words for the result
;;;    and subcalculate scratch area.
;;;
;;; E. Finally sum in u1*v1 to give the result in the first 4 D/2 words.
;;;

;; This might need some tuning
(declaim (fixnum *karatsuba-classical-cutoff*))
(defparameter *karatsuba-classical-cutoff* 16
  "When the bignum pieces are smaller than this many words, we use the
classical multiplication algorithm instead of recursing all the way
down to individual words.")

(defun karatsuba-internal (u v ret n d result)
  (declare (type bignum-index u v ret)
	   (type bignum-index d n)
	   (type bignum-type result)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((odd (logand d 1))
	 (origd d)
	 (d/2- (ash d -1))
	 (d/2 (+ d/2- odd))
	 (d (* d/2 2))
	 (n/2 (ash n -1)))
    (labels ((bignum-multiply-in-place (u v ret d result cleard)
	       (declare (type bignum-index u v ret)
			(type bignum-index d)
			(type bignum-type result)
			(optimize (speed 3) (safety 0) (debug 0)))
	       ;; Clear out the result area, which might contain garbage for
	       ;; other recursions!
	       (do ((i ret (1+ i))
		    (end (+ ret (* 2 cleard))))
		   ((>= i end))
		 (setf (%bignum-ref result i) 0))
	       ;; Stolen from bignum.lisp multiply-bignum: a straight forward
	       ;; multiplication via the classical schoolbook algorithm.
	       (dotimes (i d)
		 (declare (type bignum-index i))
		 (let ((carry-digit 0)
		       (x (%bignum-ref result (+ u i)))
		       (k (+ ret i)))
		   (declare (type bignum-index k)
			    (type bignum-element-type carry-digit x))
		   (do ((j d (1- j))
			(v+j v (1+ v+j)))
		       ((zerop j))
		     (declare (type bignum-index j v+j))
		     (multiple-value-bind (big-carry res-digit)
			 (%multiply-and-add x
					    (%bignum-ref result v+j)
					    (%bignum-ref result k)
					    carry-digit)
		       (declare (type bignum-element-type big-carry res-digit))
		       (setf (%bignum-ref result k) res-digit)
		       (setf carry-digit big-carry)
		       (incf k)))
		   (setf (%bignum-ref result k) carry-digit))))
	     (abs-diff (n1 x n2 y res result)
	       (declare (type bignum-index n1 n2)
			(type bignum-index x y res))
	       (flet ((bigger-p ()
			;; We guarantee in the caller that n1 is either
			;; n2 or (1+ n2)
			(when (> n1 n2)
;			  (assert (= n1 (1+ n2)))
			  (if (zerop (%bignum-ref result (+ x n1 -1)))
			      (progn
				(setf (%bignum-ref result (+ res n1 -1)) 0)
				(decf n1))
			      (return-from bigger-p t)))
			;; Return T if X >= Y.
			(do ((i (1- (+ x n1)) (1- i))
			     (j (1- (+ y n2)) (1- j)))
			    (())
			  (declare (type bignum-index i j))
			  (let ((x-digit (%bignum-ref result i))
				(y-digit (%bignum-ref result j)))
			    (when (/= x-digit y-digit)
			      (return-from bigger-p (>= x-digit y-digit))))
			  ;; Need to test for the exit condition here
			  ;; instead of in the DO headers because when
			  ;; X or Y is 0 and the numbers are equal I
			  ;; or J will be decremented to -1, which
			  ;; isn't of type bignum-index.
			  (when (= i x)
			    (return-from bigger-p t)))))
		 (declare (inline bigger-p))
		 (let ((sign 1))
		   (when (not (bigger-p))
		     (rotatef x y)
		     (rotatef n1 n2)
		     (setf sign -1))
		   (let ((borrow 1))
		     (do ((i x (1+ i))
			  (j y (1+ j))
			  (k res (1+ k))
			  (end (+ x n1))
			  (end2 (+ y n2)))
			 ((>= i end))
		       (declare (type bignum-index i j k end))
		       (multiple-value-bind (diff new-borrow)
			   (%subtract-with-borrow (%bignum-ref result i)
						  (if (>= j end2)
						      0
						      (%bignum-ref result j))
						  borrow)
			 (setf (%bignum-ref result k) diff)
			 (setf borrow new-borrow)))
		     sign))))
	     (propagate-carry (start end carry)
	       (declare (type bignum-type result)
			(type bignum-index start end)
			(type (integer -4 4) carry))
	       (cond ((minusp carry)
		      (multiple-value-bind (sum borrow)
			  (%subtract-with-borrow (%bignum-ref result start)
						 (- carry) 1)
			(setf (%bignum-ref result start) sum)
			(do ((i (1+ start) (1+ i)))
			    ((or (>= i end) (= borrow 1)) (- borrow 1))
			  (declare (type bignum-index i))
			  (multiple-value-bind (sum next-borrow)
			      (%subtract-with-borrow (%bignum-ref result i)
						     0 borrow)
			    (setf borrow next-borrow)
			    (setf (%bignum-ref result i) sum)))))
		     (t
		      (multiple-value-bind (sum carry)
			  (%add-with-carry (%bignum-ref result start) carry 0)
			(setf (%bignum-ref result start) sum)
			(do ((i (1+ start) (1+ i)))
			    ((or (>= i end) (zerop carry)) carry)
			  (declare (type bignum-index i))
			  (multiple-value-bind (sum next-carry)
			      (%add-with-carry (%bignum-ref result i) 0 carry)
			    (setf carry next-carry)
			    (setf (%bignum-ref result i) sum)))))))
	     (stageC-add ()
	       (let ((carry11 0)
		     (carry12 0)
		     (carry2 0))
		 (do ((i0 ret (1+ i0))
		      (i1 (+ ret d/2) (1+ i1))
		      (i2 (+ ret d) (1+ i2))
		      (i3 (+ ret n/2) (1+ i3))
		      (i4 (+ ret n/2 d/2) (1+ i4))
		      (end (+ ret d/2)))
		     ((>= i0 end))
		   (declare (type bignum-index i0 i1 i2 i3 i4))
		   (let ((b (%bignum-ref result i1)))
		     (multiple-value-bind (sum c1)
			 (%add-with-carry b (%bignum-ref result i0) carry11)
		       (multiple-value-bind (sum c2)
			   (%add-with-carry sum (%bignum-ref result i3) carry12)
			 (setf carry11 c1)
			 (setf carry12 c2)
			 (setf (%bignum-ref result i1) sum)))
		     (multiple-value-bind (sum carry)
			 (%add-with-carry b (%bignum-ref result i4) carry2)
		       (setf (%bignum-ref result i2) sum)
		       (setf carry2 carry))))
		 (+ (propagate-carry (+ ret d) (+ ret n/2) (+ carry11 carry12))
		    carry2)))
	     (stageC-subtract ()
	       (let ((carry11 0)
		     (borrow12 1)
		     (borrow2 1))
		 (do ((i0 ret (1+ i0))
		      (i1 (+ ret d/2) (1+ i1))
		      (i2 (+ ret d) (1+ i2))
		      (i3 (+ ret n/2) (1+ i3))
		      (i4 (+ ret n/2 d/2) (1+ i4))
		      (end (+ ret d/2)))
		     ((>= i0 end))
		   (declare (type bignum-index i0 i1 i2 i3 i4))
		   (let ((b (%bignum-ref result i1)))
		     (multiple-value-bind (sum c1)
			 (%add-with-carry b (%bignum-ref result i0) carry11)
		       (multiple-value-bind (sum b2)
			   (%subtract-with-borrow sum (%bignum-ref result i3)
						  borrow12)
			 (setf carry11 c1)
			 (setf borrow12 b2)
			 (setf (%bignum-ref result i1) sum)))
		     (multiple-value-bind (sum borrow)
			 (%subtract-with-borrow b (%bignum-ref result i4)
						borrow2)
		       (setf (%bignum-ref result i2) sum)
		       (setf borrow2 borrow))))
		 (+ (propagate-carry (+ ret d) (+ ret n/2)
				     (+ carry11 borrow12 -1))
		    (- borrow2 1))))
	     (stageE-add (stageC-carry)
	       ;; Sum the above pieces, result to R0124.
	       (let ((carry1 0)
		     (carry21 0)
		     (carry22 0))
		 (declare (type (mod 2) carry1 carry21 carry22))
		 (do ((i0 ret (1+ i0))
		      (i1 (+ ret d/2) (1+ i1))
		      (i2 (+ ret d) (1+ i2))
		      (i3 (+ ret d d/2) (1+ i3))
		      (i4 (+ ret d d) (1+ i4))
		      (end (+ ret d/2)))
		     ((>= i0 end))
		   (declare (type bignum-index i0 i1 i2 i3 i4))
		   (multiple-value-bind (sum c1)
		       (%add-with-carry (%bignum-ref result i1)
					(%bignum-ref result i3)
					carry1)
		     (setf carry1 c1)
		     (setf (%bignum-ref result i1) sum))
		   (let ((e (%bignum-ref result i4)))
		     (multiple-value-bind (sum c1)
			 (%add-with-carry e (%bignum-ref result i2) carry21)
		       (multiple-value-bind (sum c2)
			   (%add-with-carry sum (%bignum-ref result i3) carry22)
			 (setf carry21 c1)
			 (setf carry22 c2)
			 (setf (%bignum-ref result i2) sum)))
		     (setf (%bignum-ref result i3) e)))
;		 (assert (zerop
		 (propagate-carry (+ ret d) (+ ret d d) carry1)
;			  )
;		 (assert (zerop
		 (propagate-carry (+ ret d d/2)
				  (+ ret d d)
				  (+ carry21 carry22 stageC-carry))
		 ; )
		 )))
      (cond ((<= d *karatsuba-classical-cutoff*)
	     (bignum-multiply-in-place u v ret origd result d))
	    (t
	     (let* ((ret1 (+ ret d/2))
		    (ret3 (+ ret n/2))
		    (u-hi (+ u d/2))
		    (v-hi (+ v d/2))
		    (diff-sign (* (abs-diff d/2 u d/2- u-hi ret result)
				  (abs-diff d/2 v d/2- v-hi ret1 result))))
	       (declare (type bignum-index d/2 ret1 ret3 u-hi v-hi)
			(type (member -1 1) diff-sign))
	       (do ((i ret3 (1+ i)))
		   ((>= i (+ ret3 d)))
		 (setf (%bignum-ref result i) 0))
	       (karatsuba-internal ret ret1 ret3 n/2 d/2 result)
	       (do ((i ret (1+ i)))
		   ((>= i ret3))
		 (setf (%bignum-ref result i) 0))
	       (karatsuba-internal u v ret n/2 d/2 result)
	       (let ((stagec-carry (if (minusp diff-sign)
				       (stagec-add)
				       (stagec-subtract))))
		 (setf ret3 (+ ret d d/2))
		 (karatsuba-internal u-hi v-hi ret3 n/2 d/2- result)
		 (when (/= d/2 d/2-)
		   (setf (%bignum-ref result (+ ret3 (* d/2- 2))) 0)
		   (setf (%bignum-ref result (+ ret3 (* d/2- 2) 1)) 0))
		 (stageE-add stageC-carry))))))
    result))

(defun power-of-two (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; Compute the smallest power of two greater than or equal
  ;; to the given number.
  (declare (type bignum-index n))
  (let ((power 1))
    (declare (type bignum-index power))
    (loop while (< power n) do
	  (setf power (ash power 1)))
    power))

(defun copy-bignum-to-buffer (buffer start bignum bignum-length)
  (declare (type bignum-index start bignum-length)
	   (type bignum-type buffer bignum))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (do ((k 0 (1+ k)))
      ((>= k bignum-length))
    (setf (%bignum-ref buffer start)
	  (%bignum-ref bignum k))
    (incf start)))

;;; karatsuba  --  Interface.
;;;
;;; Multiply two bignums using the Karatsuba multiplication technique.
;;;
;;; The arguments are currently copied to the end of the working
;;; result causing some extra consing, and the arguments are both
;;; extended to have the same width implementation rather than
;;; exploiting a small width.
;;;
(defun karatsuba (x y)
  (declare (type bignum-type x y)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((x-plusp (%bignum-0-or-plusp x (%bignum-length x)))
	 (y-plusp (%bignum-0-or-plusp y (%bignum-length y)))
	 (x (if x-plusp x (negate-bignum x)))
	 (y (if y-plusp y (negate-bignum y)))
	 (negate-res (not (eq x-plusp y-plusp)))
	 (x-words (%bignum-length x))
	 (y-words (%bignum-length y))
	 (d (max x-words y-words))
	 (n (power-of-two d)))
    (declare (type bignum-index n))

    (let ((result (%allocate-bignum (* 5 n))))

      ;; Copy X, Y to the end of the result area, so that we can use
      ;; indices off of the result area for accessing everything.
      (copy-bignum-to-buffer result (* 3 n) x x-words)
      (copy-bignum-to-buffer result (* 4 n) y y-words)

      ;; Do it.
      (karatsuba-internal (* 3 n) (* 4 n) 0 (* n 3)
			  (max x-words y-words)
			  result)
      ;; Convert the result into a bignum.
      (let ((new-len (1+ (* 2 n)))
	    (n5 (* n 5)))
	(do ((k (* 2 d) (1+ k)))
	    ((>= k n5))
	  (setf (%bignum-ref result k) 0))
	(%bignum-set-length result new-len)
	(when negate-res
	  (negate-bignum-in-place result))
	(%normalize-bignum result (1+ (* 2 n)))))))

(defun classical-multiply-bignums (a b)
  (declare (type bignum-type a b))
  (let* ((a-plusp (%bignum-0-or-plusp a (%bignum-length a)))
         (b-plusp (%bignum-0-or-plusp b (%bignum-length b)))
         (a (if a-plusp a (negate-bignum a)))
         (b (if b-plusp b (negate-bignum b)))
         (len-a (%bignum-length a))
         (len-b (%bignum-length b))
         (len-res (+ len-a len-b))
         (res (%allocate-bignum len-res))
         (negate-res (not (eq a-plusp b-plusp))))
    (declare (type bignum-index len-a len-b len-res))
    (dotimes (i len-a)
      (declare (type bignum-index i))
      (let ((carry-digit 0)
            (x (%bignum-ref a i))
            (k i))
        (declare (type bignum-index k)
                 (type bignum-element-type carry-digit x))
        (dotimes (j len-b)
          (multiple-value-bind (big-carry res-digit)
              (%multiply-and-add x
                                 (%bignum-ref b j)
                                 (%bignum-ref res k)
                                 carry-digit)
            (declare (type bignum-element-type big-carry res-digit))
            (setf (%bignum-ref res k) res-digit)
            (setf carry-digit big-carry)
            (incf k)))
        (setf (%bignum-ref res k) carry-digit)))
    (when negate-res (negate-bignum-in-place res))
    (%normalize-bignum res len-res)))

(defparameter *min-karatsuba-bits* 2048
  "Use Karatsuba if the bignums have at least this many bits")

(defun new-multiply-bignums (a b)
  (declare (type bignum-type a b))
  ;; Use either the classical algorithm or the Karatsuba algorithm
  ;; depending on the size of the numbers.
  (let* ((len-a (integer-length a))
	 (len-b (integer-length b))
	 (min-len (min len-a len-b))
	 (max-len (max len-a len-b)))
    ;; If both numbers are short, use the classical algorithm.  If the
    ;; numbers vary greatly in length, use the classical algorithm.
    ;; Otherwise use the Karatsuba algorithm.
    (if (or (<= min-len *min-karatsuba-bits*)
	    (>= max-len (* 4 min-len)))
	(classical-multiply-bignums a b)
	(karatsuba a b))))

;;; END MULTIPLY

(defun test-multiply (size)
  (dotimes (i #xffff)
    (let* ((a (+ (expt 2 32) (random (expt 2 size))))
	   (b (+ (expt 2 32) (random (expt 2 size))))
	   (k (sb-bignum::new-multiply-bignums a b))
	   (c (sb-bignum::classical-multiply-bignums a b)))
      (format t ".") (force-output)
      (unless (= c k)
	(error "a=~A~%b=~A~%c=~A~%k=~A~%" a b c k)))))

(defun foo (x) x)

(defun bar (f n)
;  (gc :full t)
  (let ((a (ash #xfedc n))
	(b (ash #x1234 n))
	(start (get-internal-run-time)))
    (dotimes (i (expt 2 14)) (foo (funcall f a b)))
    (- (get-internal-run-time) start)))

(defun test-multiply-2 (&optional (a 512) (b 2048))
  (loop for i from a to b by 512 do
	(format t "~5A ~5A ~5A~%" i
		(or (gc :full t)
		    (bar #'sb-bignum::karatsuba i))
		(or (gc :full t)
		    (bar #'sb-bignum::classical-multiply-bignums i)))
	(force-output)))

(defun test-multiply-3 (&optional (a 512) (b 4096))
  (loop for i from a to b by 128 do
	(format t "~5A ~5A ~5A~%"
		i
		(or (gc :full t)
		    (bar #'sb-bignum::multiply-bignums i))
		(or (gc :full t)
		    (bar #'sb-bignum::classical-multiply-bignums i)))
	(force-output)))

(defun karat-test (n size)
  (declare (fixnum n))
  (dotimes (k n)
    (let* ((max-num (ash 1 size))
	   (x (random max-num))
	   (y (random max-num))
	   (true 0)
	   (kar 0))
      (declare (fixnum k))
      (setf true (* x y))
      (setf kar (new-multiply-bignums x y))
      (let ((bad (/= kar true)))
	(when bad
	  (format t "(- (* ~X ~X) ~X)~%" x y kar)
	  (format t "~X~%~X~%" (* x y) kar)))))
  (values))
