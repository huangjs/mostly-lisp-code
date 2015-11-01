;;;;   -*- Mode: lisp; Package: user; Syntax: Common-lisp -*-
;;
;; Copyright (C) 2005 Alain Picard
;; All rights reserved.
;;
;; Author: Alain Picard
;;
;;;; Commentary:
;;
;;  My wife got totally hooked on this stupid game, well,
;;  I had to do _something_ while she was playing the darn thing.
;;  This is what came out of it.
;;
;;  This code is placed in the public domain.  Go crazy.
;;
;;
;;
;;;; Code:

(in-package :user)

(defpackage :sudoku
  (:use :common-lisp)
  (:export  :sudoku :read-board
	    :generate-sudoku-board
	    :generate-sudoku-board-2))

(in-package :sudoku)

(defconstant +sudoku-version+ "$Revision: 1.3 $"
  "$Id: sudoku.lisp,v 1.3 2005/09/19 06:22:15 ap Exp $
   Report bugs to: alain.picard@DIE-SPAMMER-DIE-memetrics.com")

(defconstant *numbers* '(1 2 3 4 5 6 7 8 9))

(defstruct board
  (entries (make-array '(9 9) :initial-element nil)))

(defmethod print-object ((board board) stream)
  (print-sudoku-board board stream))

(defun element (board i j)
  (aref (board-entries board) i j))

(defun (setf element) (value board i j)
  (setf (aref (board-entries board) i j) value))

(defmacro bind-when ((var pred) &body body)
  `(let ((,var ,pred))
     (when ,var ,@body)))

(defmacro dotimes* (((variable range) &rest others)  &body body)
  `(dotimes (,variable ,range)
     ,@(if others
         `((dotimes* ,others ,@body))
         body)))

(defmacro do-board ((board element &optional i j) &body body)
  "Execute BODY on each square of the BOARD, binding the
   value to ELEMENT (which is SETFable), and optionally, I,J,
   the position of ELEMENT on the board."
  (let ((i (or i (gensym "INDEX")))
	(j (or j (gensym "INDEX"))))
    `(dotimes* ((,i 9) (,j 9))
      (symbol-macrolet ((,element (element ,board ,i ,j)))
	  ,@body))))

(defun copy-board (board)
  (let ((new-board (make-board)))
    (do-board (board e i j)
      (setf (element new-board i j) e))
    new-board))

(defun print-sudoku-board (board &optional (stream *standard-output*))
  (flet ((print-line ()
	   (format stream "~&+-------+-------+-------+~%")))
    (dotimes (i 9)
      (when (zerop (mod i 3))
	(print-line))
      (loop for j below 9 by 3 do
	    (format stream "| ~:[ ~;~:*~D~] ~:[ ~;~:*~D~] ~:[ ~;~:*~D~] "
		    (element board i (+ 0 j))
		    (element board i (+ 1 j))
		    (element board i (+ 2 j))))
      (format stream "|~%"))
    (print-line)))

(defun solved-p (board)
  (do-board (board e)
    (when (null e)
      (return-from solved-p nil)))
  t)

(defun read-board (stream)
  "Read a sudoku board from STREAM.  Pretty much anything is
   allowed; I like to write boards something like this:

	;;; Board 60,

	x x 5   x x 9  x 1 x
	x 2 x   x x x  x x x
	x x 9   x x 6  3 x x

	8 x x   x x x  x 7 x
	x x x   x x x  x x x
	4 x 3   x x x  9 x x

	2 3 x   4 x x  x x x
	7 x x   9 x 8  x x 3
	x 1 x   x x 5  6 x 8"
  (let ((board (make-board)))
    (do-board (board element x y)
      (let ((e (read stream t nil)))
	(unless (and (integerp e) (<= 1 e 9))
	  (setf e nil))
	(setf element e)))
    board))

;; A `move' is a list of 3 elements, (I J ELEMENT)
(defun make-move (board move)
  "Return a new board with MOVE applied to BOARD."
  (let ((board (copy-board board)))
    (make-move! board move)
    board))

(defun make-moves! (board moves)
  (dolist (move moves)
    (make-move! board move))
  board)

(defun make-move! (board move)
  (destructuring-bind (i j element) move
    (setf (element board i j) element)))

(defun collect-row (board row)
  (loop for j below 9
	when (element board row j)
	collect it))

(defun collect-column (board col)
  (loop for i below 9
	when (element board i col)
	collect it))

(defun collect-square (board i j)
  (let ((x (* 3 (floor j 3)))
	(y (* 3 (floor i 3)))
	(elements '()))
    (dotimes* ((i 3) (j 3))
      (bind-when (e (element board (+ y i) (+ x j)))
	(push e elements)))
    elements))

(defun guesses (board i j)
  "Return a list of all possible values which can be validly placed at {I J}.
   If this value is already filled, the keyword :KNOWN is
   present in the guesses, and the resulting list has length 2."
  (bind-when (e (element board i j))
    ;; Already known.
    (return-from guesses (list e :known)))
  (let* ((guesses *numbers*)
	 (guesses (set-difference guesses (collect-row board i)))
	 (guesses (set-difference guesses (collect-column board j)))
	 (guesses (set-difference guesses (collect-square board i j))))
    guesses))

(defun find-certain-moves (board)
  "Return all moves which are fully constrained on BOARD."
  (let ((moves '()))
    (do-board (board e i j)
      (unless e
	(let ((guesses (guesses board i j)))
	  (when (= 1 (length guesses))	; certain move
	    (push (list i j (first guesses)) moves)))))
    moves))

(defun execute-certain-moves (board)
  "Reduce a sudoku BOARD by looping and executing all currently
   known certain moves."
  (loop as moves = (find-certain-moves board)
	while moves
	do (make-moves! board moves))
  board)

(defun possible-new-moves (board)
  (let ((guesses '()))
    (do-board (board e i j)
      (let ((guess (guesses board i j)))
	(unless (find :known guess)
	  (push (list i j guess) guesses))))
    (nreverse guesses)))

(defun sort-moves (moves)
  "Sort the possible moves keeping those with the
   smallest number of choices near the start."
  (stable-sort moves
	       (lambda (m1 m2)
		 (< (length (third m1))
		    (length (third m2))))))

(defun best-move-to-try (board)
  (first
   (sort-moves
    (possible-new-moves board))))

(defun sudoku (board)
  "Solve a SUDOKU board."
  ;; The algorithm is pretty simple:
  ;; fill in all certain moves, then, when we get
  ;; to a branch point, try every guess, calling ourselves
  ;; recursively on the board with the certain moves
  ;; and the guess filled in.
  (let ((board (copy-board board)))
    (setf board (execute-certain-moves board))
    (cond ((solved-p board)
	   board)
	  (t
	   (destructuring-bind (i j guesses) (best-move-to-try board)
	     (loop for guess in guesses
		   as new-board = (sudoku (make-move board (list i j guess)))
		   while new-board ; If fails, board is invalid.
		   until (solved-p new-board)
		   finally return (or new-board board)))))))

;; Stolen from somewhere, but pretty sure he stole it too.  :-)
(defmethod shuffle! ((l list) &optional (start 0))
  ;; this declaration compiles into pretty fast (and reckless :-) code
  (declare (optimize (debug 0) (safety 0) (space 0) (speed 3)))
  (let ((len (length l)))
    (do ((rest (nthcdr start l) (cdr rest))
	 (posn start (1+ posn)))
	((null rest) l)
      (let ((swaposn (random (- len posn))))
	(when (not (zerop swaposn))
	  (rotatef (car (nthcdr swaposn rest)) (car rest)))))))

(defun generate-sudoku-board (&optional board)
  (flet ((make-random-move-list (board)
	   (shuffle! (possible-new-moves board))))
    (let ((board (or board (make-board))))
      (setf board (execute-certain-moves board))
      (cond ((solved-p board)
	     board)
	    (t
	     (destructuring-bind (i j elements) (first (make-random-move-list board))
	       (loop for e in elements
		     as new-board = (generate-sudoku-board (make-move board (list i j e)))
		     while new-board
		     until (solved-p new-board)
		     finally return (or new-board board))))))))


;; Another attempt --- this one lets you specify how many
;; squares you want filled in.  This should keep her amused,
;; oh, I don't know... forever?
(defun generate-sudoku-board-2 (&optional (n-filled 15) (board (make-board)))
  (flet ((get-random-move (board)
	   (let ((guesses (shuffle! (possible-new-moves board))))
	     (loop for (i j moves) in guesses
		   until moves
		   finally return
		   (list i j (nth (random (length moves)) moves))))))
    (cond ((zerop n-filled)
	   board)
	  (t
	   (make-move! board (get-random-move board))
	   (generate-sudoku-board-2 (1- n-filled)
				    board)))))

#+(or)
(with-open-file (s "board.1")
   (print-sudoku-board
    (sudoku (read-board s))))

;;; SUDOKU.LISP ends here
