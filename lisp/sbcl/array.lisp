;;; sbcl array experiment.

;;; helper function
(defun assign (array value)
  (loop for i from 0 below (array-total-size array)
	 do (setf (row-major-aref array i) value)))

(defun check (array value)
  (loop for i from 0 below (array-total-size array)
	 do (when (/= value (aref array i))
		  (return-from check nil))
	 finally (return t)))

(defun make-simple-array (dimensions type)
  (make-array dimensions :element-type type))


;;; template of testing functions
(defmacro define-array-fns (type-of-element)

  `(progn  
	 ;; 2 dimensional (unsigned-byte 64) array
	 ;; normal, i & j increasing
	 (defun test1 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from 0 below (first (array-dimensions src))
		  do (loop for j of-type fixnum from 0 below (second (array-dimensions src))
				do (setf (aref dest i j)
						 (aref src i j)))))

	 ;; i decreasing j increasing
	 (defun test2 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from (the fixnum (1- (the fixnum (first (array-dimensions src))))) downto 0
		  do (loop for j of-type fixnum from 0 below (second (array-dimensions src))
				do (setf (aref dest i j)
						 (aref src i j)))))

	 ;; i increasing, j decreasing
	 (defun test3 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from 0 below (first (array-dimensions src))
		  do (loop for j of-type fixnum from (the fixnum (1- (the fixnum (second (array-dimensions src))))) downto 0
				do (setf (aref dest i j)
						 (aref src i j)))))
	 
	 ;; i decreasing j decreasing
	 (defun test4 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from (the fixnum (1- (the fixnum (first (array-dimensions src))))) downto 0
		  do (loop for j of-type fixnum from (the fixnum (1- (the fixnum (second (array-dimensions src))))) downto 0
				do (setf (aref dest i j)
						 (aref src i j)))))

	 ;; i decreasing j decreasing but change the accessing sequence
	 (defun test5 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from (the fixnum (1- (the fixnum (first (array-dimensions src))))) downto 0
		  do (loop for j of-type fixnum from (the fixnum (1- (the fixnum (second (array-dimensions src))))) downto 0
				do (setf (aref dest j i)
						 (aref src j i)))))

	 ;; row-major-aref, i increasing
	 (defun test6 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from 0 below (array-total-size src)
		  do (setf (row-major-aref src i) (row-major-aref dest i))))

	 ;; row-major-aref, i decreasing
	 (defun test7 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from (1- (array-total-size src)) downto 0
		  do (setf (row-major-aref src i) (row-major-aref dest i))))

	 ;; i decreasing j decreasing + manually unrolled loop
	 (defun test8 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from (the fixnum (1- (the fixnum (first (array-dimensions src))))) downto 0
		  do (loop for j of-type fixnum from (the fixnum (1- (the fixnum (second (array-dimensions src))))) downto 0
				do (progn
					 (setf (aref dest i j) (aref src i j))
					 (decf j)
					 (setf (aref dest i j) (aref src i j))
					 (decf j)
					 (setf (aref dest i j) (aref src i j))
					 (decf j)
					 (setf (aref dest i j) (aref src i j))))))

	 ;; row-major-aref, i increasing + manually unrolled loop
	 (defun test9 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from 0 below (array-total-size src)
		  do (progn
			   (setf (row-major-aref dest i) (row-major-aref src i))
			   (incf i)
			   (setf (row-major-aref dest i) (row-major-aref src i))
			   (incf i)
			   (setf (row-major-aref dest i) (row-major-aref src i))
			   (incf i)
			   (setf (row-major-aref dest i) (row-major-aref src i)))))

	 ;; row-major-aref, i decreasing + manually unrolled loop
	 (defun test10 (src dest)
	   (declare (optimize speed (safety 0))
				((simple-array ,type-of-element (* *)) src dest))
	   (loop for i of-type fixnum from (1- (array-total-size src)) downto 0
		  do (progn
			   (setf (row-major-aref dest i) (row-major-aref src i))
			   (decf i)
			   (setf (row-major-aref dest i) (row-major-aref src i))
			   (decf i)
			   (setf (row-major-aref dest i) (row-major-aref src i))
			   (decf i)
			   (setf (row-major-aref dest i) (row-major-aref src i)))))

	 ))

;;; summary:
;;; test1 : i increasing, j increasing, (aref array i j)
;;; test2 : i decreasing, j increasing, (aref array i j)
;;; test3 : i increasing, j decreasing, (aref array i j)
;;; test4 : i decreasing, j decreasing, (aref array i j)
;;; test5 : i decreasing, j decreasing, *** (aref array j i)
;;; test6 : row-major-aref, i increasing
;;; test7 : row-major-aref, i decreasing
;;; test8 : i decreasing, j decreasing, manually unrolled loop, (aref array i j)
;;; test9 : row-major-aref, i increasing, manually unrolled loop
;;; test10 : row-major-aref, i decreasing, manually unrolled loop

;;; testing, with different types
(define-array-fns (unsigned-byte 64))
(defparameter ubyte-64-x (make-simple-array '(4000 4000) '(unsigned-byte 64)))
(defparameter ubyte-64-y (make-simple-array '(4000 4000) '(unsigned-byte 64)))
(assign ubyte-64-x 200)
(gc :full t)
(time (test1 ubyte-64-x ubyte-64-y))	;0.107
(time (test2 ubyte-64-x ubyte-64-y))	;0.106
(time (test3 ubyte-64-x ubyte-64-y))	;0.105
(time (test4 ubyte-64-x ubyte-64-y))	;0.105
(time (test5 ubyte-64-x ubyte-64-y))	;0.44
(time (test6 ubyte-64-x ubyte-64-y))	;0.105
(time (test7 ubyte-64-x ubyte-64-y))	;0.106
(time (test8 ubyte-64-x ubyte-64-y))	;0.106
(time (test9 ubyte-64-x ubyte-64-y))	;0.105
(time (test10 ubyte-64-x ubyte-64-y))	;0.106
(setf ubyte-64-x nil
	  ubyte-64-y nil)
(gc :full t)


(define-array-fns (unsigned-byte 32))
(defparameter ubyte-32-x (make-simple-array '(4000 4000) '(unsigned-byte 32)))
(defparameter ubyte-32-y (make-simple-array '(4000 4000) '(unsigned-byte 32)))
(assign ubyte-32-x 200)
(gc :full t)
(time (test1 ubyte-32-x ubyte-32-y))	;0.103
(time (test2 ubyte-32-x ubyte-32-y))	;0.073
(time (test3 ubyte-32-x ubyte-32-y))	;0.071
(time (test4 ubyte-32-x ubyte-32-y))	;0.072
(time (test5 ubyte-32-x ubyte-32-y))	;0.284
(time (test6 ubyte-32-x ubyte-32-y))	;0.053
(time (test7 ubyte-32-x ubyte-32-y))	;0.053
(time (test8 ubyte-32-x ubyte-32-y))	;0.058
(time (test9 ubyte-32-x ubyte-32-y))	;0.052
(time (test10 ubyte-32-x ubyte-32-y))	;0.052
(setf ubyte-32-x nil
	  ubyte-32-y nil)
(gc :full t)


(define-array-fns (unsigned-byte 128))	; => (simple-array t (* *))
(defparameter ubyte-128-x (make-simple-array '(4000 4000) '(unsigned-byte 128)))
(defparameter ubyte-128-y (make-simple-array '(4000 4000) '(unsigned-byte 128)))
(assign ubyte-128-x 200)
(gc :full t)
(time (test1 ubyte-128-x ubyte-128-y))	;0.106
(time (test2 ubyte-128-x ubyte-128-y))	;0.106
(time (test3 ubyte-128-x ubyte-128-y))	;0.106
(time (test4 ubyte-128-x ubyte-128-y))	;0.107
(time (test5 ubyte-128-x ubyte-128-y))	;0.438
(time (test6 ubyte-128-x ubyte-128-y))	;0.106
(time (test7 ubyte-128-x ubyte-128-y))	;0.106
(time (test8 ubyte-128-x ubyte-128-y))	;0.106
(time (test9 ubyte-128-x ubyte-128-y))	;0.106
(time (test10 ubyte-128-x ubyte-128-y))	;0.105
(setf ubyte-128-x nil
	  ubyte-128-y nil)
(gc :full t)


(define-array-fns (unsigned-byte 16))
(defparameter ubyte-16-x (make-simple-array '(4000 4000) '(unsigned-byte 16)))
(defparameter ubyte-16-y (make-simple-array '(4000 4000) '(unsigned-byte 16)))
(assign ubyte-16-x 200)
(gc :full t)
(time (test1 ubyte-16-x ubyte-16-y))	;0.067
(time (test2 ubyte-16-x ubyte-16-y))	;0.067
(time (test3 ubyte-16-x ubyte-16-y))	;0.068
(time (test4 ubyte-16-x ubyte-16-y))	;0.068
(time (test5 ubyte-16-x ubyte-16-y))	;0.225
(time (test6 ubyte-16-x ubyte-16-y))	;0.027
(time (test7 ubyte-16-x ubyte-16-y))	;0.027
(time (test8 ubyte-16-x ubyte-16-y))	;0.051
(time (test9 ubyte-16-x ubyte-16-y))	;0.027
(time (test10 ubyte-16-x ubyte-16-y))	;0.028
(setf ubyte-16-x nil
	  ubyte-16-y nil)
(gc :full t)


(define-array-fns (unsigned-byte 8))
(defparameter ubyte-8-x (make-simple-array '(4000 4000) '(unsigned-byte 8)))
(defparameter ubyte-8-y (make-simple-array '(4000 4000) '(unsigned-byte 8)))
(assign ubyte-8-x 200)
(gc :full t)
(time (test1 ubyte-8-x ubyte-8-y))	;0.067
(time (test2 ubyte-8-x ubyte-8-y))	;0.067
(time (test3 ubyte-8-x ubyte-8-y))	;0.068
(time (test4 ubyte-8-x ubyte-8-y))	;0.068
(time (test5 ubyte-8-x ubyte-8-y))	;0.193
(time (test6 ubyte-8-x ubyte-8-y))	;0.022
(time (test7 ubyte-8-x ubyte-8-y))	;0.022
(time (test8 ubyte-8-x ubyte-8-y))	;0.052
(time (test9 ubyte-8-x ubyte-8-y))	;0.022
(time (test10 ubyte-8-x ubyte-8-y))	;0.022
(setf ubyte-8-x nil
	  ubyte-8-y nil)
(gc :full t)


(define-array-fns (unsigned-byte 4))
(defparameter ubyte-4-x (make-simple-array '(4000 4000) '(unsigned-byte 4)))
(defparameter ubyte-4-y (make-simple-array '(4000 4000) '(unsigned-byte 4)))
(assign ubyte-4-x 1)
(gc :full t)
(time (test1 ubyte-4-x ubyte-4-y))		;0.138
(time (test2 ubyte-4-x ubyte-4-y))		;0.144
(time (test3 ubyte-4-x ubyte-4-y))		;0.144
(time (test4 ubyte-4-x ubyte-4-y))		;0.147
(time (test5 ubyte-4-x ubyte-4-y))		;0.2
(time (test6 ubyte-4-x ubyte-4-y))		;0.114
(time (test7 ubyte-4-x ubyte-4-y))		;0.115
(time (test8 ubyte-4-x ubyte-4-y))		;0.134
(time (test9 ubyte-4-x ubyte-4-y))		;0.113
(time (test10 ubyte-4-x ubyte-4-y))		;0.119
(setf ubyte-4-x nil
	  ubyte-4-y nil)
(gc :full t)


(define-array-fns bit)
(defparameter ubyte-bit-x (make-simple-array '(4000 4000) 'bit))
(defparameter ubyte-bit-y (make-simple-array '(4000 4000) 'bit))
(assign ubyte-bit-x 1)
(gc :full t)
(time (test1 ubyte-bit-x ubyte-bit-y))		;0.132
(time (test2 ubyte-bit-x ubyte-bit-y))		;0.135
(time (test3 ubyte-bit-x ubyte-bit-y))		;0.134
(time (test4 ubyte-bit-x ubyte-bit-y))		;0.134
(time (test5 ubyte-bit-x ubyte-bit-y))		;0.166
(time (test6 ubyte-bit-x ubyte-bit-y))		;0.116
(time (test7 ubyte-bit-x ubyte-bit-y))		;0.117
(time (test8 ubyte-bit-x ubyte-bit-y))		;0.127
(time (test9 ubyte-bit-x ubyte-bit-y))		;0.117
(time (test10 ubyte-bit-x ubyte-bit-y))		;0.122
(setf ubyte-bit-x nil
	  ubyte-bit-y nil)
(gc :full t)


