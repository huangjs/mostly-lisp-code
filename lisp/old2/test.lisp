(defun fact (n)
  (if (= n 1)
	  1
	  (* n (fact (- n 1)))))

(defun cfact (n k)
  (if (= n 1)
	  (funcall k 1)
	  (cfact (- n 1)
			 (lambda (result)
			   (funcall k (* n result))))))

(defun ifact (n)
  (labels ((iter (n acc)
				 (if (= n 1)
					 acc
					 (iter (1- n) (* acc n)))))
	(iter n 1)))

(defun lfact (n)
  (loop for i from 1 to n
	 with result = 1
	 do (progn
		  (setf result (* result i)))))

(defun test ()
  (let ((counter 0))
	(labels ((start-counting ()
			   (incf counter)))
	  #'start-counting)))



;;;;;;;;;;;;;;

;;; a producer is a function that accepts a collection, a consumer and
;;; a continuation that continue producing the rest after it finishes.
;;; a consumer is a function that accepts an element, a flag of EOF
;;; and a continuation that will provide more values.  a rest-producer
;;; (actually, it's producers) is a function/continuation that accepts
;;; a consumer. it will call producer recursively and continue the
;;; producing one by one. It produces on-demand.
(defun producer (x consumer rest-producer)
  (cond ((null x)
		 (funcall rest-producer consumer))
		((atom x)
		 (funcall consumer x nil rest-producer)) ; nil stands for not EOF
		(t
		 (producer (car x)
				   consumer
				   #'(lambda (consumer)
					   (producer (cdr x) consumer rest-producer))))))

;;; here we use rest-producer as input because it fully encapsulate
;;; the state of current collection. What we need to do is to define a
;;; consumer to get the current value because we compare two
;;; collections, thus we need two consumers which get element from x
;;; and y respectively. We need to chain (nest) them together because
;;; the actual logic of doing need to see the scope of both
;;; value. Here, a consumer just accept value without doing anything
;;; to it. The (or ...) form is the semantics of samefringec but not
;;; the consumer.
(defun samefringec (xg yg)
  (funcall xg
   #'(lambda (x eofx rest-producer-x)
	   (funcall yg
		#'(lambda (y eofy rest-producer-y)
			;; the logic of samefringec
			(or (and eofx eofy)
				(and (eql x y)
					 (samefringec rest-producer-x rest-producer-y))))))))

;;; finally we can define our function. The initial rest-producer is a
;;; predicate that tells whether the collection is empty. 
(defun samefringe (x y)
  (samefringec #'(lambda (consumer)
				   (producer x consumer #'(lambda (c) (funcall c nil t nil))))
			   #'(lambda (consumer)
				   (producer y consumer #'(lambda (c) (funcall c nil t nil))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; benchmark result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; we also defined (copied actually) another two version of samefringe.
;;; samefringe1 fully evaluate x and y, while samefringe2 evaluate x only and replace y with a generator.

;;; first we define two arbitrary trees with same fringe.
(setf x  '(1 (((3))) 3 4 (1 ((((((((((1)))))))))) 1 2) 3 (((6))) 7 (((8))) 9))
(setf y (copy-tree x))

;;; Here's the result
(time (repeating 100000 (samefringe1 x y)))
=>
Evaluation took:
  0.249 seconds of real time
  0.245034 seconds of user run time
  0.003144 seconds of system run time
  [Run times include 0.02 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  20,804,784 bytes consed.

(time (repeating 100000 (samefringe2 x y)))
=>
Evaluation took:
  0.369 seconds of real time
  0.328661 seconds of user run time
  0.007224 seconds of system run time
  [Run times include 0.051 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  63,210,840 bytes consed.

(time (repeating 100000 (samefringe x y))) ; our fully lazy version
=>
Evaluation took:
  0.611 seconds of real time
  0.593754 seconds of user run time
  0.017643 seconds of system run time
  [Run times include 0.125 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  164,810,200 bytes consed.


;;; the result shows our CPS version is only 1.5x slower than the fastest version.

;;; next test: x and y are slightly different in an early stage in the
;;; sense of depth first iteration
(setf x  '(1 (((2))) 3 4 (1 ((((((((((1)))))))))) 1 2) 3 (((6))) 7 (((8))) 9))
               ^^^
           originally 3
;;; y remains unchanged

(time (repeating 100000 (samefringe1 x y)))
=>
Evaluation took:
  0.206 seconds of real time
  0.203446 seconds of user run time
  0.001819 seconds of system run time
  [Run times include 0.011 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  20,814,176 bytes consed.

;;; half evaled version is now faster than fully evaled version
(time (repeating 100000 (samefringe2 x y)))
=>
Evaluation took:
  0.173 seconds of real time
  0.142524 seconds of user run time
  0.029071 seconds of system run time
  [Run times include 0.038 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  18,406,568 bytes consed.

;;; lazy version now becomes the fastest.
(time (repeating 100000 (samefringe x y)))
=>
Evaluation took:
  0.083 seconds of real time
  0.080427 seconds of user run time
  0.003147 seconds of system run time
  [Run times include 0.018 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  27,210,336 bytes consed.


;;; Afterthoughts

;; The larger the collection is, the more expensive the operation
;; (comparison here) is on the data, the less the overhead of our CPS
;; approach. The importance is that this approach makes functions
;; composable. The producer is actually very general, it has no logic
;; of what will be done to the data, it only on-demand iterate the
;; list in depth first order. The iteration sequence can be
;; parameterized so it can be used as a general interface of lazy
;; enumeration. The samefringe and samefringec also can be abstracted
;; into a macro, so that we only need to define the logic of
;; samefringe and samefringec can also be a local function of
;; samefringe. Thus this way of programming is desired in CL.

