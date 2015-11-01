(defpackage :continuo
  (:use :cl :cl-user))

(in-package :continuo)

(defparameter *number-of-cards* 42)

(defun initializ-board ()
  (make-array (list (+ 8 (* 2 4 *number-of-cards*))
					(+ 8 (* 2 4 *number-of-cards*)))
			  :initial-element nil))

(defparameter *board* (initializ-board)
  "The game board. used places are represented as keywords, which represent colors, like :RED or :YELLOW, etc. unused places are marked with NIL")

(defparameter *orig-x-offset* (/ (array-dimension *board* 0) 2))
(defparameter *orig-y-offset* (/ (array-dimension *board* 1) 2))

(defun block-color (i j)
  (aref *board* (+ i *orig-x-offset*) (+ j *orig-y-offset*)))

(defun (setf block-color) (val i j)
  (setf (aref *board* (+ i *orig-x-offset*) (+ j *orig-y-offset*))
		val))

(defun cross-product (&rest args)
  "(cross-product '(1 2 3) '(x y z) '(aa bb cc))
=>
\((3 Z CC) (3 Y BB) (3 X AA) (2 Z CC) (2 Y BB) (2 X AA) (1 Z CC) (1 Y BB)
 (1 X AA))"
  (let ((length (length args)))
	(case length
	  (0 '())
	  (1 (first args))
	  (2 (let ((result '()))
		   (dolist (x (first args))
			 (dolist (y (second args))
			   (push (list x y) result)))
		   result))
	  (otherwise
	   (let ((result '()))
		 (dolist (e (first args))
		   (dolist (l (apply #'cross-product (rest args)))
			 (push (cons e l) result)))
		 result)))))

(defun build-hash-table (key-value-pairs)
  "make a hash table from an assoc list."
  (let ((table (make-hash-table :size (ceiling (* 1.5 (length key-value-pairs))))))
	(dolist (e key-value-pairs)
	  (setf (gethash (car e) table) (cdr e)))
	table))

(defparameter *colors* '(:red :yellow :blue :green))
(defparameter *color-abbrevs*
  (build-hash-table '((:red . "R")
					  (:blue . "B")
					  (:yellow . "Y")
					  (:green . "G"))))

(defparameter *card-color-lookup-table*
  (build-hash-table
   (mapcar #'(lambda (l)
			   (cons (intern
					  (apply #'concatenate 'string
							 (mapcar #'(lambda (e)
										 (gethash e *color-abbrevs*))
									 l))
					  :keyword)
					 l))
		   (cross-product *colors* *colors* *colors* *colors*)))
  "A lookup table that has keys of color abbreviations like :RBRB and values of the corrosponding list of color names, such as (:RED :BLUE :RED :BLUE)")

(defun make-card (card)
  (destructuring-bind (ll ml mr rr)
	  (gethash card *card-color-lookup-table*)
	(let ((m (list (list ll ml mr rr)
				   (list ml ml mr mr)
				   (list mr mr ml ml)
				   (list rr mr ml ll))))
	  (make-array '(4 4) :initial-contents m))))

(defparameter *card-color-pattern-lookup-table*
  (build-hash-table
   (let ((result '()))
	 (maphash #'(lambda (key val)
				  (declare (ignore val))
				  (push (cons key (make-card key)) result))
			  *card-color-lookup-table*)
	 result)))

(defstruct (rectangle (:conc-name nil)
					  (:constructor rec (left right up down))) 
  left right up down)

(defparameter *possible-range* (rec 0 0 0 0)
  "The rectangle range that need to be considered. Outside this range, there's no need to check for placement.")

(defparameter *valid-positions* '(0 0))

(defparameter *played-cards* '()
  "A list of played cards, the element is a list in the form of ((x y) card-color), where x and y is the coordination of top-left block of the card and card-color is the abbreviation form such as :RBRB.")

(defun export-board-to-ps (&optional (filename "continuo.ps"))
  "Prints the board to a postscript file, zooming automatically.
The paper size of the output is A4 (8.3 x 11.7 inches)."
  (let* ((total-width (* 8.3 72))
		 (total-height (* 11.7 72))
		 (width (- (right *possible-range*) (left *possible-range*)))
		 (height (- (down *possible-range*) (up *possible-range*)))
		 (edge-length (min (/ total-width width) (/ total-height height)))
		 (x-margin (* (- total-width (* width edge-length)) 0.5))
		 (y-margin (* (- total-height (* height edge-length)) 0.5))
		 (colors '((:red (1 0 0)) (:green (0 1 0))
				   (:blue (0 0 1)) (:yellow (1 1 0)))))
	(with-open-file (s filename :direction :output :if-exists :supersede)
	  (format s "%!PS~%~
                 /box {~% ~
                  ~f~:* 0 rlineto 0 -~f~:* rlineto -~f 0 rlineto closepath ~%~
                 } def~%" edge-length)
	  (dotimes (i width)
		(dotimes (j height)
		  (let* ((bi (+ i (left *possible-range*)))
				 (bj (+ j (up *possible-range*)))
				 (color (second (assoc (block-color bi bj) colors))))
			(when color
			  (format s "newpath~% ~
                          ~f ~f moveto box gsave~% ~
                          ~{~f ~}setrgbcolor fill~% ~
                          grestore stroke~%"
					  (+ (* i edge-length) x-margin)
					  (- total-height (* j edge-length)  y-margin)
					  color)))))
	  (format s "showpage~%"))))

;;; entry
(defun clear ()
  (setf *board* (initializ-board))
  (setf *possible-range* (rec 0 0 0 0))
  (setf *played-cards* '()))

(defun play (card)
  (if (null *played-cards*)
	  (play-first-hand card)
	  (play-next card)))

(defun play-first-hand (card)
  (let ((position '(0 0)))
	(push (list position card) *played-cards*) 
	(update-board position card)
	(list 0 0 nil 0)				; no values
	))

(defun update-board (position card)
  (labels ((update-line (position line-of-colors)
			 (let ((x (first position))
				   (y (second position)))
			   (loop for i from 0 below 4
				  for color in line-of-colors
				  do (setf (block-color (+ x i) y) color)))))
	(let ((x (first position))
		  (y (second position)))
	  ;; update the board
	  (destructuring-bind (ll ml mr rr)
		  (gethash card *card-color-lookup-table*)
		(update-line (list x y) (list ll ml mr rr))
		(update-line (list x (1+ y)) (list ml ml mr mr))
		(update-line (list x (+ 2 y)) (list mr mr ml ml))
		(update-line (list x (+ 3 y)) (list rr mr ml ll)))
	  ;; update the possible range
	  (update-possible-range position)
	  (update-valid-positions))))

(defun update-possible-range (position)
  (let ((x (first position))
		(y (second position)))
	(setf *possible-range*
		  (rec (min (- x 4) (left *possible-range*))
			   (max (+ x 8) (right *possible-range*))
			   (min (- y 4) (up *possible-range*))
			   (max (+ y 8) (down *possible-range*))))))

(defun update-valid-positions ()
  (labels ((empty-p (pos)
			 (destructuring-bind (x y) pos
			   (block exit
				 (dotimes (i 4 t)
				   (dotimes (j 4)
					 (when (block-color (+ i x) (+ j y))
					   (return-from exit nil)))))))
		   (valid-p (pos)
			 (and (empty-p pos)
				  (destructuring-bind (x y) pos
					(block exit
					  (dotimes (i 4 nil)
						(when (or (block-color (+ x i) (- y 1))
								  (block-color (+ x i) (+ y 4))
								  (block-color (- x 1) (+ i y))
								  (block-color (+ x 4) (+ i y)))
						  (return-from exit t))))))))
	(let ((result '())
		  (width (- (right *possible-range*) (left *possible-range*)))
		  (height (- (down *possible-range*) (up *possible-range*))))
	  (dotimes (i width)
		(dotimes (j height)
		  (when (valid-p (list i j))
			(push (list i j) result))))
	  (setf *valid-positions* result))))

;;; play next
(defun play-next (card)
  (destructuring-bind (move spin-p score)
	  (choose-best-position card)
	(push (list move card) *played-cards*)
	(update-board move card)
	(list (first move) (second move) spin-p score)))

;;; FIXME:
(defun choose-best-position (card)
  (apply #'max
		 (mapcar (lambda (card position)
				   (evaluate card position))
				 *valid-positions*)))

(defun evaluate (card position)
  "return 0 if not placable. try spin also"
  (let ((pattern (gethash card *card-color-pattern-lookup-table*))
		(mark (make-array '(4 4) :initial-element nil))
		(result 0)) ;mark for calculated
	(labels ((calculate-length (i j)
			   ))
	  (dotimes (i 4)
		(dotimes (j 4)
		  (incf result (calculate-length i j)))))))

