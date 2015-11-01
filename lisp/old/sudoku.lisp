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
   :content						 ;!!! it should be set as not setfable
   :region						 ;!!! it should be set as not setfable
   :make-square-region
   :get-block))

(in-package :sudoku-puzzle)
   
    
;;;; Data representation

;;;; CONSTRUCTOR and ACCESSOR for a single BLOCK
;;; BLOCK is made of two things: the CONTENT it hold and the REGION it locates.
;;; here we use a simple pair as the data structure.
;;; e.g. (2 . 1) means block that is holding number 2 in region 1.
;;; therefore, you can make any kind of region on the map.
(defun make-block (&key content region)
  (cons content region))

;;; they're all SETFABLE.
(defun content (a-block)
  (car a-block))

(defun set-content-of-block (a-block value)
  (setf (car a-block) value))

(defsetf content set-content-of-block)

(defun region (a-block)
  (cdr a-block))

(defun set-region-of-block (a-block value)
  (setf (cdr a-block) value))

(defsetf region set-region-of-block)


;;;; CONSTRUCTOR and ACCESSOR of a single REGION
;;; we only build square region here, s.t.
;;; we use 2-dimension ARRAY to represent the data.
(defmacro make-square-region (&key size region)
  `(make-array '(,size ,size) :initial-element '(() ,region) :allocation t))  ;make sure all of it's element is nil so that we can compare.

;;; BLOCK is a special form, it cannot be overrided.
;;; s.t. we use get-block and set-block.
(defmacro get-block (region &key row col)
  `(aref ,region (1- ,row) (1- ,col)))

(defmacro set-block (region &key row col block)
  `(setf (aref ,region (1- ,row) (1- ,col)) ,block))


;;;; CONSTRUCTOR and ACCESSOR of a PUZZLE
;;; a PUZZLE is composed of REGIONs.
;;; PUZZLE maintains a new array of block for quick access.
;;; PUZZLE also contains a hashtable for finding the region of a block quickly.
;;; !! but data changes in the puzzle should also change the value in the corresponding region.
;;; !! user should NEVER set value in a region directly.
(defmacro make-puzzle 
