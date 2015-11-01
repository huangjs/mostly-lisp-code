(in-package :cl-user)  
;; in the IDE (in-package :cg-user)
(require :process)

(defun factorial (n) (if (< n 2) n (* n (factorial (1- n)))))

;; This lock is used to prevent output interleaving.
(defvar moby-output-lock (mp:make-process-lock))

;; Print to the stream the number of trailing
;; zeros in (factorial n) from n=from up to n=to.
;; This is a *very* inefficient way to do this computation,
;; but the point is to make it run slow enough to see.

(defun process-test (stream from to)
  (do ((n from (1+ n))) 
      ((>= n to)) 
    (do ((x (factorial n) (/ x 10)) 
         (zeros -1 (1+ zeros))) 
        ((not (integerp x)) 
	 (mp:with-process-lock (moby-output-lock) 
	   (format stream "factorial(~d) has ~d trailing zeros~%"
		   n zeros))
	 )))) 

;; This starts three processes in parallel. 
;; The original Lisp listener returns immediately, 
;; and will accept types forms while the other processes run. 

(defun moby-process-test () 
  (mp:process-run-function "Test 1" #'process-test t 400 440)
  (mp:process-run-function "Test 2" #'process-test t 440 470)
  (mp:process-run-function "Test 3" #'process-test t 470 400) 
  t) 

;; Make sure factorial itself is compiled 
;; because large factorials exceed the interpreter's stack. 

(unless (compiled-function-p #'factorial) (compile 'factorial)) 

(format t "Type (moby-process-test) to test multi-processing.~%") 
