;;;; sudoku-puzzle2.lisp -- A Lisp sudoku solver
;;; Copyright (c) 2006 Jianshi Huang

;;; DATE		VERSION		AUTHOR			COMMENT
;;; 06/05/19	0.1			Huang			Beginning, everything is fine. Poor programming style, primitive experiment.
;;;										 	See Wikipedia: sudoku

(proclaim '(optimize (debug 0) (safety 0) (space 0) (speed 3)))
(proclaim '(type (simple-array fixnum (9 9)) board))
(proclaim '(type list *numbers*))
(proclaim '(inline valid? choose-randomly valid-numbers))

;;;; Code:

(defconstant +board-size+ 9   "Rows and cols of a sudoku puzzle board")
(defconstant +region-size+ 3   "Rows and cols of a region")
(defconstant +empty+ 0   "Empty cell marker")

(defparameter *numbers* '(1 2 3 4 5 6 7 8 9)		"Numbers to be filled.")

;;; Example
;;; This is the example board. Fill it with the puzzle you want to solve
;;; Copied from Wikipedia.
;;; You can specify any kind of board you like.
(defparameter board #2A((0 0 0 0 0 0 0 0 0)
						(0 0 0 0 0 0 9 0 0)
						(9 7 0 3 0 0 0 0 0)
						(0 1 0 0 6 0 5 0 0)
						(0 0 4 7 0 8 0 0 2)
						(0 0 0 0 0 2 0 0 6)
						(0 3 1 0 0 4 0 0 0)
						(0 0 0 8 0 0 1 6 7)
						(0 8 7 0 0 0 0 0 0)))

(defparameter board-backup (copy-board board))  ; make a backup

;;; After loading the file please run:
;;; (guess 0) -> to find a solution
;;; (print-board) -> to print the solution


;;;; The algorithm to solve a sudoku puzzle!

;;; Find a solution
;;; e.g. (guess! 0) guess! from index 0.
;;; index ranges from 0 to (1- (* +board-size+ +board-size+))
;;; which here is 0 to 80
;;; Using simple backtracking, not very efficient.
(defun guess! (index)
  "Test all candidate numbers for current cell until board is complete."
  (let ((row (floor (/ index +board-size+)))
        (col (mod index +board-size+)))
    (if (not (array-in-bounds-p board row col))
        (return-from guess! t))                     ;if out of range, then it means the board has been filled and a solution has been found.
    (if (/= (aref board row col) +empty+)
        (guess! (1+ index))              ;guess the next cell if not empty
        ;; Test all numbers from 1 to 9
        (loop for i in *numbers* 
              do (if (valid? i row col)
                     (progn
                       (setf (aref board row col) i)
                       (if (guess! (1+ index))
                           (return t))))
              ;; Backtrack
              finally (progn
                        (setf (aref board row col) +empty+)
                        (return nil))))))

(defun valid? (num row col)
  "Check if a number is a legal candidate for the cell in (row, col)"
  ;; Get the top left corner (row, col) of a 3x3 region
  (let ((r (* (floor (/ row +region-size+)) +region-size+))
        (c (* (floor (/ col +region-size+)) +region-size+)))
    (dotimes (i +board-size+ t)         ;return t after scanning all the possible cells.
      (if (or (= num (aref board row i))  ; row check
              (= num (aref board i col))  ; col check
              (= num (aref board (+ r (mod i +region-size+))
                               	 (+ c (floor (/ i +region-size+))))))  ; region check
          (return nil)))))

;;; !! should use format control string, but not very important here.
(defun print-board ()
  "Pretty print the board"
  (dotimes (r +board-size+)
    (format t "~&|")
    (dotimes (c +board-size+)
      (format t " ~A" (aref board r c)))
    (format t " |")))


;;;; Support functions

;;; Copied from somewhere else.
(defmethod shuffle! ((l list) &optional (start 0))
  "Shuffle a sequence/list, destructive."
  ;; this declaration compiles into pretty fast (and reckless :-) code
  (declare (optimize (debug 0) (safety 0) (space 0) (speed 3)))
  (let ((len (length l)))
    (do ((rest (nthcdr start l) (cdr rest))
	 (posn start (1+ posn)))
	((null rest) l)
      (let ((swaposn (random (- len posn))))
	(when (not (zerop swaposn))
	  (rotatef (car (nthcdr swaposn rest)) (car rest)))))))

;;; Copy a board
(defun copy-board (board)
  "Make a copy of the board"
  (hjs-array:copy-array board))

;;; restore the board
(defun restore-board! ()
  "Restore the board from board-backup."
  (defparameter board (copy-board board-backup)))

;;; make-board!
;;; set both board and board-backup
;;; !!! ambiguous meaning
(defun make-board! (&optional (new-board nil))
  "Make an empty board if no template provided, or set the board to the given template."
  (if (null new-board)
      ;; !! expose the details of data structure, very bad!
      (setf new-board (make-array '(9 9) :element-type 'integer :initial-element 0)))  ;make an empty board
  (progn
    (defparameter board (copy-board new-board))
    (defparameter board-backup (copy-board board)))
  new-board)

;;; make-example-board!
(defun make-example-board! ()
  "Set the board to the example from wikipedia."
  (let ((example-board #2a((5 3 0 0 7 0 0 0 0)
                           (6 0 0 1 9 5 0 0 0)
                           (0 9 8 0 0 0 0 6 0)
                           (8 0 0 0 6 0 0 0 3)
                           (4 0 0 8 0 3 0 0 1)
                           (7 0 0 0 2 0 0 0 6)
                           (0 6 0 0 0 0 2 8 0)
                           (0 0 0 4 1 9 0 0 5)
                           (0 0 0 0 8 0 0 7 9))))
    (declare (type (simple-array fixnum (9 9)) example-board))
    (defparameter board (copy-board example-board)
    (defparameter board-backup (copy-board example-board)))
    board))


;;;; Other solvers

;;; solve the game board with a random solution
(defun random-solver! ()
  (let ((*numbers* (shuffle! (copy-list *numbers*))))  ;shadow, the original *numbers* doesn't change
    (guess! 0)
    (print-board)))


;;;; Generate game board

;;; Generate a random game board with number of cells specified (optional).
;;; Randomness is assured. But ...
;;; !!! Broken, might generate invalid board.
(defun generate-board (&optional (ncells 30))
  (let ((assign-sequence                ;generate initial sorted sequence
         (loop for index from 0 to (1- (* +board-size+ +board-size+))
               collect (let ((row (floor (/ index +board-size+)))
                             (col (mod index +board-size+)))
                         (list row col))))
        (board (make-array '(9 9) :element-type 'integer :initial-element 0))      ;make an empty board to be filled in
        (numbers (copy-list *numbers*)))           
    ;; shuffle it and get the assignment sequence within ncells!
    (setf assign-sequence (subseq (shuffle! assign-sequence) 0 ncells))
    (loop for cell in assign-sequence
          do (let ((row (first cell))
                   (col (second cell)))
               ;; set the cell randomly with valid numbers based on current board situation.
               (setf (aref board row col)
                     (choose-randomly (valid-numbers numbers row col)))))         ;might be nil.
    board)) 

(defun choose-randomly (list)
  "Randomly choose an element from a given list."
  (if (null list)
      nil
      (first (shuffle! list))))

(defun valid-numbers (numbers row col)
  "Collect all the valid numbers from a given list of numbers in the current cell of (row, col)."
  (loop for i in numbers when (valid? i row col)
        collect i))


