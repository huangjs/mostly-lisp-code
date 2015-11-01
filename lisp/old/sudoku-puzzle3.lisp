;;;; sudoku-puzzle3.lisp -- A Lisp sudoku solver

;;; DATE		VERSION		AUTHOR			COMMENT
;;; 06/05/19	0.1			Huang			Beginning, everything is fine. Poor programming style, experimental.
;;;										 	See Wikipedia: sudoku
;;; 06/05/20	0.2			Huang			Add goodness to every cell, sorted before guess, add goodness to each number on each cell, also sorted.


;;;; Code:

(defconstant +board-size+ 9   "Rows and cols of a sudoku puzzle board")
(defconstant +region-size+ 3   "Rows and cols of a region")
(defconstant +empty+ 0   "Empty cell marker")

(defparameter *numbers* '(1 2 3 4 5 6 7 8 9)		"Numbers to be filled.")

;;; Example
;;; This is the example board. Fill it with the puzzle you want to solve
;;; Copied from Wikipedia.
;;; You can specify any kind of board you like.
;; (defparameter board #2a((5 3 0 0 7 0 0 0 0)
;;                         (6 0 0 1 9 5 0 0 0)
;;                         (0 9 8 0 0 0 0 6 0)
;;                         (8 0 0 0 6 0 0 0 3)
;;                         (4 0 0 8 0 3 0 0 1)
;;                         (7 0 0 0 2 0 0 0 6)
;;                         (0 6 0 0 0 0 2 8 0)
;;                         (0 0 0 4 1 9 0 0 5)
;;                         (0 0 0 0 8 0 0 7 9)))

(defparameter board #2A((0 0 0 0 0 0 0 0 0)
			(0 0 0 0 0 0 9 0 0)
			(9 7 0 3 0 0 0 0 0)
			(0 1 0 0 6 0 5 0 0)
			(0 0 4 7 0 8 0 0 2)
			(0 0 0 0 0 2 0 0 6)
			(0 3 1 0 0 4 0 0 0)
			(0 0 0 8 0 0 1 6 7)
			(0 8 7 0 0 0 0 0 0)))

(defparameter *hard-sudoku* #2a((7 0 0 0 0 0 0 1 9)
                                (4 6 0 1 9 0 0 0 0)
                                (0 0 0 6 8 2 7 0 4)
                                (0 9 0 0 0 0 0 0 7)
                                (0 0 0 3 0 0 4 0 5)
                                (0 0 6 7 0 0 0 0 0)
                                (0 0 1 0 0 0 0 0 0)
                                (2 0 0 0 7 4 0 0 0)
                                (0 0 0 2 0 0 3 0 0)))

;;; After loading the file please run:
;;; (solve)

;;;; The algorithm to solve a sudoku puzzle!

;;; Find a solution
(defun solve ()
  "Solve the puzzle, generate an optimal search space first."
  (let* ((board (hjs.data.array:copy-array board))
         (ref-board (mark-board board)) ; To marke the board with all possible moves on each cell.
         (guess-sequence (sort (collect-index #'listp ref-board)
                               #'< :key #'cell-goodness)))
					;                               #'> :key (cell-goodness2 ref-board))))  ; Get the optimal guess sequence.
    ;; Ok, finally we gonna try to solve the puzzle by guessing.
    (if (guess! board ref-board guess-sequence)
        board
        nil)))

(defun mark-board (board)
  "Mark the board with all possible moves in each cell."
  (let ((new-board (make-array `(,+board-size+ ,+board-size+) :adjustable nil)))
    (loop for row from 0 below +board-size+ do
	 (loop for col from 0 below +board-size+ do
	      (let ((cell (aref board row col)))
		(if (empty-cell? cell)
		    (setf (aref new-board row col) (valid-numbers *numbers* row col))
		    (setf (aref new-board row col) cell)))))
					;    (sort-numbers-in-cell! new-board)
    new-board))

(defun sort-numbers-in-cell! (board)
  (loop for row from 0 below +board-size+ do
       (loop for col from 0 below +board-size+ do
	    (let ((cell (aref board row col)))
	      (when (listp cell)
		(setf (aref board row col)
		      (sort cell #'> :key (number-goodness board row col)))))))
  board)

(defun empty-cell? (cell)
  "Return true if cell is empty, otherwise return nil."
  (eql +empty+ cell))

(defun collect-index (pred board)
  "Collect the indices of elements in an array when pred is true."
  (let ((indices nil))
    (loop for row from 0 below +board-size+ do
	 (loop for col from 0 below +board-size+ do
	      (let ((cell (aref board row col)))
		(if (apply pred (list cell))
		    (push (list row col) indices)))))
    (reverse indices)))

(defun cell-better-than? (cell-x cell-y)
  "Return true if cell-x is a better or equal candidate than cell-y,
otherwise, return nil. "
  (let ((x (cell-goodness cell-x))
        (y (cell-goodness cell-y)))
    ;; safe or strict? --> we choose safe here
    (<= x y)))

(defun guess! (board ref-board guess-sequence)
  "Test all candidate numbers according to the given sequence of guess until board is complete."
  (let* ((index (first guess-sequence))
         (row (first index))
         (col (second index)))
    (if (null guess-sequence)
        (return-from guess! t)) ;if out of range, then it means the board has been filled and a solution has been found.
    (loop for num in (aref ref-board row col) do
	 (when (valid? num row col)
	   (setf (aref board row col) num)
	   (if (guess! board ref-board (rest guess-sequence))
	       (return t)))
       ;; Backtracking
	 finally
	 (progn
	   (setf (aref board row col) +empty+)
	   (return nil)))))

(defun solved-board? (board)
  (dotimes (row +board-size+ t)
    (dotimes (col +board-size+)
      (when (empty-cell? (aref board row col))
        (return-from solved-board? nil)))))                          

(defun valid? (num row col)
  "Check if a number is a legal candidate for the cell in (row, col)"
  ;; Get the top left corner (row, col) of a 3x3 region
  (let ((r (* (floor (/ row +region-size+)) +region-size+))
        (c (* (floor (/ col +region-size+)) +region-size+)))
    (dotimes (i +board-size+ t) ;return t after scanning all the possible cells.
      (if (or (= num (aref board row i))  ; row check
              (= num (aref board i col))  ; col check
              (= num (aref board (+ r (mod i +region-size+))
                           (+ c (floor (/ i +region-size+)))))) ; region check
          (return nil)))))

;;; safe order filter
(defun valid-numbers (numbers row col)
  "Collect all the valid numbers from a given list of numbers in the current cell of (row, col)."
  (loop for i in numbers when (valid? i row col)
     collect i))

;;; (row, col)  -> int
(defun cell-goodness (cell-index)
  "The goodness of cells. The bigger the goodness is,
the sooner the cell should be guessed."
  (let ((row (first cell-index))
        (col (second cell-index)))
    (length (valid-numbers *numbers* row col))))

(defun cell-goodness2 (board)
  #'(lambda (position)
      (let ((row (first position))
            (col (second position)))
        (reduce #'+ (mapcar (number-goodness board row col)
                            (aref board row col))
                :initial-value 0))))

;;; (num, row, col) -> int
(defun number-goodness (board row col)
  " Goodness of number in a particular cell.
the bigger the goodness is, the sooner the number should be guessed."
  #'(lambda (num)
      ;; Get the top left corner (row, col) of a 3x3 region
      (let ((r (* (floor (/ row +region-size+)) +region-size+))
            (c (* (floor (/ col +region-size+)) +region-size+))
            (goodness 0))
        (dotimes (i +board-size+)
          (let ((row-cell (aref board row i))
                (col-cell (aref board i col))
                (reg-cell (aref board (+ r (mod i +region-size+))
				(+ c (floor (/ i +region-size+))))))
            (when (and (listp row-cell)
                       (member num row-cell))
              (incf goodness))
            (when (and (listp col-cell)
                       (member num col-cell))
              (incf goodness))
            (when (and (listp reg-cell)
                       (member num reg-cell))
              (incf goodness))))
        goodness)))


;;; Format string should be used, but not very important.
(defun print-board (board)
  "Pretty print the board"
  (dotimes (r +board-size+)
    (format t "~&|")
    (dotimes (c +board-size+)
      (format t " ~A" (aref board r c)))
    (format t " |")))

;;; Save and read function
(defun save-board (board comment)
  (with-open-file (out "sudoku-board.txt"
                       :direction :output
                       :if-exists :append)
    (with-standard-io-syntax
      (print (list board comment) out))))


