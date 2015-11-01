;;;; sudoku-screamer.lisp -- A sudoku solver using Screamer
;;; you need to load both screamer and screamer+

(screamer:define-screamer-package :sudoku
    (:use :iterate :cl :screamer+))

(in-package :sudoku)

(defconstant +empty+ 0   "Empty cell marker")

;;; example from wikipedia
(defparameter *simple-sudoku* #2a((5 3 0 0 7 0 0 0 0)
				  (6 0 0 1 9 5 0 0 0)
				  (0 9 8 0 0 0 0 6 0)
				  (8 0 0 0 6 0 0 0 3)
				  (4 0 0 8 0 3 0 0 1)
				  (7 0 0 0 2 0 0 0 6)
				  (0 6 0 0 0 0 2 8 0)
				  (0 0 0 4 1 9 0 0 5)
				  (0 0 0 0 8 0 0 7 9)))

(defparameter *hard-sudoku* #2a((7 0 0 0 0 0 0 1 9)
				(4 6 0 1 9 0 0 0 0)
				(0 0 0 6 8 2 7 0 4)
				(0 9 0 0 0 0 0 0 7)
				(0 0 0 3 0 0 4 0 5)
				(0 0 6 7 0 0 0 0 0)
				(0 0 1 0 0 0 0 0 0)
				(2 0 0 0 7 4 0 0 0)
				(0 0 0 2 0 0 3 0 0)))

;;; just constraint propagation
;;; best first + constraint propagation
(defun find-solution-best-first (variable-array fitness-fn order-fn)
  (one-value
   (solution variable-array
	     (reorder fitness-fn (constantly nil) order-fn #'linear-force))))

;;; fitness function example, (NOTE: not optimal)
(declaim (inline make-fn-count-reducible-size))
(locally (declare (optimize (speed 3)))
  (defun make-fn-count-reducible-size (board)
    (declare ((simple-array t (9 9)) board))
    (lambda (var)
      (block return
	(let ((factor 1d0)
	      (i (car (screamer+::variable+-name var)))
	      (j (cdr (screamer+::variable+-name var)))
	      (next-guess (first (screamer+::variable+-enumerated-domain var))))
	  (declare (double-float factor)
		   ((integer 0 8) i j)
		   ((integer 1 9) next-guess))
	  (labels ((update-factor (cell)
		     (let* ((domain (screamer+::variable+-enumerated-domain cell))
			    (size (length (the list domain))))
		       (when (member next-guess domain)
			 (if (= size 1)
			     (return-from return most-positive-fixnum)
			     (setf factor (* factor (/ size (1- size)))))))))
	    (iter (for x below 9)
		  (let ((cell (aref board i x)))
		    (unless (or (numberp cell) (= x j))
		      (update-factor cell)))
		  (let ((cell (aref board x j)))
		    (unless (or (numberp cell) (= x i))
		      (update-factor cell))))
	    (when (and (> i 0) (> j 0))
	      (let ((cell (aref board (1- i) (1- j))))
		(unless (numberp cell)
		  (update-factor cell))))
	    (when (and (< i 8) (> j 0))
	      (let ((cell (aref board (1+ i) (1- j))))
		(unless (numberp cell)
		  (update-factor cell))))
	    (when (and (> i 0) (< j 8))
	      (let ((cell (aref board (1- i) (1+ j))))
		(unless (numberp cell)
		  (update-factor cell))))
	    (when (and (< i 8) (< j 8))
	      (let ((cell (aref board (1+ i) (1+ j))))
		(unless (numberp cell)
		  (update-factor cell))))
	    (/ factor (domain-size var))))))))

;;; generate array with logic variables
(defun make-constraint-array (board)
  (let ((constraint-array (make-array '(9 9))))
    ;; initialization
    (iter (for i below 9)
	  (iter (for j below 9)
		(if (eql +empty+ #1=(aref board i j))
		    (setf (aref constraint-array i j) (an-integer-betweenv 1 9 (cons i j)))
		    (setf (aref constraint-array i j) #1#))))
    ;; set up constraints
    (iter (for i below 9)
	  ;; row constraints
	  (assert! (/=v (aref constraint-array i 0)
			(aref constraint-array i 1)
			(aref constraint-array i 2)
			(aref constraint-array i 3)
			(aref constraint-array i 4)
			(aref constraint-array i 5)
			(aref constraint-array i 6)
			(aref constraint-array i 7)
			(aref constraint-array i 8)))
	  ;; column constraints
	  (assert! (/=v (aref constraint-array 0 i)
			(aref constraint-array 1 i)
			(aref constraint-array 2 i)
			(aref constraint-array 3 i)
			(aref constraint-array 4 i)
			(aref constraint-array 5 i)
			(aref constraint-array 6 i)
			(aref constraint-array 7 i)
			(aref constraint-array 8 i)))
	  ;; grid constraints
	  (multiple-value-bind (x y)
	      (floor i 3)
	    (let ((x (* x 3))
		  (y (* y 3)))
	      (assert! (/=v (aref constraint-array (+ x 0) (+ y 0))
			    (aref constraint-array (+ x 0) (+ y 2))
			    (aref constraint-array (+ x 1) (+ y 1))
			    (aref constraint-array (+ x 2) (+ y 0))
			    (aref constraint-array (+ x 2) (+ y 2)))))))
    constraint-array))

;;; solver

(defun solve (board)
  (let ((constraint-array (make-constraint-array board)))
    (time (print (find-solution-best-first constraint-array #'domain-size #'<)))
    (finish-output)
;;     (time (print (find-solution-best-first
;; 		  constraint-array
;; 		  (make-fn-count-reducible-size constraint-array)
;; 		  #'>)))
;;     (finish-output)
    'done))



;;; REMARK: in this example, domain-size as fitness-fn seems better
;;; than my customized function. maybe because the function is not
;;; optimized and conses a lot.
;;; Basically, domain-size is good enough for sudoku solver.
