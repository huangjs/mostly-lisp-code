;;;;; Programs for Sudoku puzzle

#|

DATE		VERSION		AUTHOR			COMMENT
06/05/19	0.1			Huang			Beginning, everything is fine.

|#

(in-package :cl-user)

(defpackage "SUDOKU-PUZZLE"
    (:use :cl)
  (:export
   :make-block
   :content                             ;!!! it should be set as not setfable
   :region                              ;!!! it should be set as not setfable
   :make-region
   :get-block))

(in-package :sudoku-puzzle)
   
;;;; Global variable
(defconstant +board-size+ 9
  "Size of board means the number of rows or the number of columns.
A board is a square of grids and the size will be +board-size+ * +board-size+.")

(defconstant +region-size+ 3
  "Size of a region, = #row or #col.
So the number of cells in a region will be +region-size+ * +region-size+.")

(defconstant +region-number+ 9
  "Number of regions in a board.")

(defconstant +region-per-row+ 3
  "Number of regions per row.")

(defconstant +empty-cell+ 0
  "Number that represents a empty cell.")

(defconstant +marked-cell+ -1
  "Number that represents a marked cell.
A marked cell means the cell cannot be filled temporarily until it's freed.")

(defconstant +numbers+ '(1 2 3 4 5 6 7 8 9)
  "Candidate numbers.")

;;;; Data representation

;;;; CONSTRUCTOR and ACCESSOR of a board
(defun make-board ()
  "This will create an empty 9*9 grid filled with +empty-cell+"
  (make-array (list +board-size+ +board-size+) :element-type 'integer :initial-element +empty-cell+ :allocation t))

(defun copy-board (board)
  "Make a copy of the board."
  (cg.base:copy-array board))

;;; ROW and COL start from 0 to +board-size+
;;; e.g. (cell board :row 0 :col 2) -> the cell value of the first line second element of a board.
(defun cell (board &key row col)
  "Get the CELL value of given ROW and COLUMN."
  (aref board row col))

(defun set-cell (board value &key row col)
  "Set the CELL of ROW and COLUMN to VALUE."
  (setf (aref board row col) value))

;;; make it SETFABLE
;;; e.g. (setf (cell board :row 0 :col 2) 8)
(defsetf cell (board &key row col) (value)
  `(set-cell ,board ,value :row ,row :col ,col))

;;; region number starts from 0 to +region-number+
;;; e.g. (region :row 0 :col 2) -> 0
;;; e.g. (region :row 3 :col 5) -> 4
(defun region (&key row col)
  "Get the region number of a given cell"
  (+ (* +region-per-row+ (floor (/ row +region-size+)))
     (floor (/ col +region-size+))))


;;;; Finding solutions
(defun find-a-solution (&key board numbers)
  "find a solution to a given board"
  (let ((board (copy-board board))
        (numbers (copy-list +numbers+)))
    (dolist (number numbers)
      (cond ((null numbers)
             (print-board board))       ;TBI
            ((Nplace :board board :number number)  
             (progn
               (find-a-solution :board (mark-number :board board :number number)
                                :number (set-difference (list number) numbers))))
            (t (return nil))))))

;;; destructive place
(defun Nplace (&key board number)
  "Place a number in the board.
Mark all related lines and columns to +marked-cell+.
If not placable, return nil."
  (dotimes (i +board-size+)
    (dotimes (j +board-size+)               ;iteration over the board
      (and (Nplace-row :board board :row i :number number)           ;TBI
           (Nplace-col :board board :col j :number number)           ;TBI
           (Nplace-region :board board :row i :col j :number number)))))      ;TBI

(defun Nplace-row (&key board row number)
  "Place a number in the ROW, if cannot, return nil."
  (dotimes (col +board-size+)
    (let ((cell (cell board :row 0 :col col)))
      (cond ((= cell number) (return nil))
            ((= cell +empty-cell+) (setf (cell board :row row :col col) +marked-cell+))
            (t nil)))))

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun Nplace-col (&key board col number)
  "Place a number in the COL, if cannot, return nil."
  (dotimes (row +board-size+)
    (if (= (cell board :row row :col col)
           number)
        nil
        (setf (cell board :row row :col col) +marked-cell+))))

(defun Nplace-region (&key board row col number)
  "Place a number in the region where cel (i, j) locates,
if cannot, return nil."
  (let* ((region (region :row row :col col))
         (start-row (* +region-size+ (floor (/ region +region-per-row+))))
         (end-row (+ start-row +region-size+))
         (start-col (* +region-size+ (mod region +region-size+)))
         (end-col (+ start-col +region-size+)))
    (loop i from start-row to end-row do
          (loop j from start-col to end-col do
                ...))))

                
