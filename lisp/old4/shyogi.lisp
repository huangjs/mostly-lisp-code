;;; -*- Mode: lisp -*-

;;;; shyogi program

;;; Copyright (C) Jianshi, Huang, Huang, Jianshi
;;; All rights reserved.
;;; See the LICENCE file for details.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-user::asdf :ltk))

(defpackage :shyogi
  (:use :cl :iterate :hjs.meta.essential :hjs.util.lisp-unit))

(in-package :shyogi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (install-hjs-lib)
  (use-package :ltk)
  (setf iterate::*always-declare-variables* t))

(defnewconstant +fu+ 0)
(defnewconstant +ka+ 1)
(defnewconstant +hi+ 2)
(defnewconstant +gi+ 3)
(defnewconstant +ki+ 4)
(defnewconstant +ou+ 5)
(defnewconstant +to+ 6)
(defnewconstant +um+ 7)
(defnewconstant +ry+ 8)
(defnewconstant +ng+ 9)

(defnewconstant +my-side+ 100)
(defnewconstant +op-side+ 101)

(defun id->unit-name (id)
  (let ((table #.(make-array 10 :element-type 'symbol
				:initial-contents
				'(歩 角 飛車 銀 金 王 と金 龍馬 龍王 成金))))
    (aref table id)))

(defun num->side (num)
  (case num
    (100 'my-side)
    (101 'op-side)))

(deftype int () '(integer -1000 1000))

(defstruct (unit (:conc-name nil))
  (kind 0 :type int)
  (side 100 :type int))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type 'unit :identity nil)
    (princ " KIND: " stream)
    (princ (id->unit-name (kind unit)) stream)
    (princ " SIDE: " stream)
    (princ (num->side (side unit)) stream)))

(defun my-side-p (unit)
  (= (side unit) +my-side+))

(defun upgradable-p (unit)
  (<= (kind unit) 3))

(defstruct (pos (:conc-name nil)
		(:constructor pos (x y))
		(:type vector))
  (x 0 :type int)
  (y 0 :type int))

(defparameter *mode* :running)
(defparameter *board* (make-array '(5 5) :initial-element nil))
(defparameter *my-slot* (make-array 5 :initial-element nil))
(defparameter *op-slot* (make-array 5 :initial-element nil))
(defparameter *gui-board* nil)
(defparameter *gui-my-slot* nil)
(defparameter *gui-op-slot* nil)

(defun reset-game ()
  ;; update board
  (setf *board* (make-array '(5 5) :initial-element nil))
  (setf (aref *board* 4 4) (make-unit :kind +ou+ :side +op-side+))
  (setf (aref *board* 4 3) (make-unit :kind +fu+ :side +op-side+))
  (setf (aref *board* 3 4) (make-unit :kind +ki+ :side +op-side+))
  (setf (aref *board* 2 4) (make-unit :kind +gi+ :side +op-side+))
  (setf (aref *board* 1 4) (make-unit :kind +ka+ :side +op-side+))
  (setf (aref *board* 0 4) (make-unit :kind +hi+ :side +op-side+))
  (setf (aref *board* 0 0) (make-unit :kind +ou+ :side +my-side+))
  (setf (aref *board* 0 1) (make-unit :kind +fu+ :side +my-side+))
  (setf (aref *board* 1 0) (make-unit :kind +ki+ :side +my-side+))
  (setf (aref *board* 2 0) (make-unit :kind +gi+ :side +my-side+))
  (setf (aref *board* 3 0) (make-unit :kind +ka+ :side +my-side+))
  (setf (aref *board* 4 0) (make-unit :kind +hi+ :side +my-side+))

  ;; update slots
  (setf *op-slot* (make-array 5 :initial-element nil))
  (setf *my-slot* (make-array 5 :initial-element nil))
  
  ;; update gui board
  (iter (for i below 5)
  	(iter (for j below 5)
  	      (for e = (aref *board* i j))
  	      (setf (getf  (aref *gui-board* i j) :unit) e)))
  
  (refresh-board))



(defun start-game ()
  (sleep 2))

(defun back ()
  )

(defun forward ()
  )

(defun set-game ()
  )

(defun try-move (from to)
  (let ((from-unit (aref *board* (x from) (y from)))
	(to-unit (aref *board* (x to) (y to))))
    (setf (aref *board* (x to) (y to)) from-unit)
    (setf (aref *board* (x from) (y from)) to-unit)
    (refresh-board)
    (refresh-board)))

;;; gui
(defun start ()
  (with-ltk ()
    (let* ((main-frame (make-instance 'frame))
	   (mn (make-menubar))
	   (m-game (make-menu mn "Game"))
	   (m-reset (make-instance
		     'menubutton
		     :master m-game
		     :text "Reset Game"
		     :command 'reset-game))
	   (m-quit (make-instance
		    'menubutton
		    :master m-game
		    :text "Quit"
		    :command (lambda () (setf *exit-mainloop* t))))
	   (l-turn (make-instance
		    'label
		    :master main-frame
		    :text "Your turn ...     "
		    :foreground :blue))
	   (b-start (make-instance
		     'button
		     :master main-frame
		     :text "Start"
		     :command (lambda ()
				(setf (text l-turn)
				      "Running, please wait...")
				(start-game)
				(setf (text l-turn)
				      "Waiting your turn ...    "))))
	   (b-set (make-instance
		   'button
		   :master main-frame
		   :text "Set"
		   :command (lambda ()
			      (setf (text l-turn)
				    "Setting the game ...    ")
			      (set-game))))
	   (b-back (make-instance
		    'button
		    :master main-frame
		    :text "<< Back"
		    :command 'back))
	   (b-forward (make-instance
		       'button
		       :master main-frame
		       :text "Forward >>"
		       :command 'forward))
	   (c-board (make-instance
		     'canvas
		     :width 400
		     :height 400
		     :background :darkgreen))
	   (c-my-slot (make-instance
		       'canvas
		       :width 400
		       :height 80
		       :background :darkblue))
	   (c-op-slot (make-instance
		       'canvas
		       :width 400
		       :height 80
		       :background :darkblue))
;; 	   (l-sep-1 (make-instance
;; 		     'label
;; 		     :master c-board
;; 		     :text "相手の持ち駒"))
;; 	   (l-sep-2 (make-instance
;; 		     'label
;; 		     :master c-board
;; 		     :text "持ち駒"))
	   ;; not widget
	   (board
	    (make-array '(5 5) :initial-element nil))
	   (my-slot
	    (make-array 5 :initial-element nil))
	   (op-slot
	    (make-array 5 :initial-element nil))
	   (select nil)
	   )
      (declare (ignorable mn m-reset m-quit m-game))
      (setf *gui-board* board)
      (setf *gui-my-slot* my-slot)
      (setf *gui-op-slot* op-slot)
      (pack (list main-frame b-start b-set b-back b-forward l-turn)
	    :expand t :fill :both :padx 10 :pady 10)
      (pack main-frame :expand t :fill :both :side :left)
      (pack c-op-slot)
      ;;(pack l-sep-1)
      (pack c-board)
      ;;(pack l-sep-2)
      (pack c-my-slot)
      

      ;; board
      (dotimes (i 5)
	(dotimes (j 5)
	  (setf (aref board i j)
		(list :widget (make-instance
			       'label
			       :master c-board
			       :background :darkgreen
			       :relief :groove
			       :width 10
			       :height 5)
		      :unit nil
		      :pos (pos i j)))
	  (grid (getf (aref board i j) :widget) j (- 4 i))
	  (bind (getf (aref board i j) :widget) "<ButtonPress-1>"
		(let ((current (aref board i j)))
		  (lambda (e)
		    (declare (ignorable e))
		    (cond ((and (null select) (getf current :unit))
			   (setf select current)
			   (configure (getf current :widget) :background :blue))
			  (select
			   (configure (getf select :widget) :background :darkgreen)
			   (try-move (getf select :pos) (getf current :pos))
			   (setf select nil))
			  (t ;; do nothing
			   nil)))))))

      ;; slots
      ;; FIXME: not binded
      (dotimes (i 5)
	(setf (aref my-slot i)
	      (list :widget (make-instance
			     'label
			     :master c-my-slot
			     :background :darkblue
			     :relief :groove
			     :width 10
			     :height 5)
		    :unit nil))
	(setf (aref op-slot i)
	      (list :widget (make-instance
			     'label
			     :master c-op-slot
			     :background :darkblue
			     :relief :groove
			     :width 10
			     :height 5)
		    :unit nil))
	(grid (getf (aref my-slot i) :widget) 0 i)
	(grid (getf (aref op-slot i) :widget) 0 i)
	)
      
      (reset-game))))

(defun unit-display-name (unit)
  (case (kind unit)
    (#.+fu+ "歩")
    (#.+hi+ "飛 車")
    (#.+ka+ "角")
    (#.+gi+ "銀 将")
    (#.+ki+ "金 将")
    (#.+ou+ "王")
    (#.+to+ "と")
    (#.+um+ "龍馬")
    (#.+ry+ "龍王")
    (#.+ng+ "成銀")))

(defun refresh-board ()
  ;; board
  (iter (for i below 5)
	(iter (for j below 5)
	      (for d = (aref *board* i j))
	      (for e = (aref *gui-board* i j)) 
	      (let ((widget (getf e :widget))
		    (unit (getf e :unit)))
		(setf (getf e :unit) d)
		(if unit
		    (progn
		      (setf (text widget) (unit-display-name unit))
		      (configure widget :foreground (if (my-side-p unit) :yellow :red)))
		    (progn
		      (setf (text widget) "")
		      (configure widget :foreground :darkgreen))))))
  ;; my-slot
  (iter (for i below 5)
	(for d = (aref *my-slot* i))
	(for e = (aref *gui-my-slot* i))
	(let ((widget (getf e :widget))
	      (unit (getf e :unit)))
	  (setf (getf e :unit) d)
	  (if unit
	      (progn
		(setf (text widget) (unit-display-name unit))
		(configure widget :foreground (if (my-side-p unit) :yellow :red)))
	      (progn
		(setf (text widget) "")
		(configure widget :foreground :darkblue)))))
  ;; op-slot
  (iter (for i below 5)
	(for d = (aref *op-slot* i))
	(for e = (aref *gui-op-slot* i))
	(let ((widget (getf e :widget))
	      (unit (getf e :unit)))
	  (setf (getf e :unit) d)
	  (if unit
	      (progn
		(setf (text widget) (unit-display-name unit))
		(configure widget :foreground (if (my-side-p unit) :yellow :red)))
	      (progn
		(setf (text widget) "")
		(configure widget :foreground :darkblue))))))

;;; ai
(declaim (inline check up dn lf rt ul ur dl dr my-side-p))

(locally (declare (optimize (speed 3) (safety 0)))
  (defun check (pos)
    (declare (dynamic-extent pos))
    (let ((x (x pos))
	  (y (y pos)))
      (if (and (>= x 0) (<= x 4)
	       (>= y 0) (<= y 4))
	  pos
	  nil)))

  (defun up (unit pos)
    (declare (dynamic-extent pos unit))
    (if (my-side-p unit)
	(check (pos (x pos) (1+ (y pos))))
	(check (pos (x pos) (1- (y pos))))))

  (defun dn (unit pos)
    (declare (dynamic-extent pos unit))
    (if (my-side-p unit)
	(check (pos (x pos) (1- (y pos))))
	(check (pos (x pos) (1+ (y pos))))))

  (defun lf (unit pos)
    (declare (dynamic-extent unit pos))
    (if (my-side-p unit)
	(check (pos (1- (x pos)) (y pos)))
	(check (pos (1+ (x pos)) (y pos)))))

  (defun rt (unit pos)
    (declare (dynamic-extent unit pos))
    (if (my-side-p unit)
	(check (pos (1+ (x pos)) (y pos)))
	(check (pos (1- (x pos)) (y pos)))))

  (defun ul (unit pos)
    (declare (dynamic-extent unit pos))
    (if (my-side-p unit)
	(check (pos (1- (x pos)) (1+ (y pos))))
	(check (pos (1+ (x pos)) (1- (y pos))))))

  (defun ur (unit pos)
    (declare (dynamic-extent unit pos))
    (if (my-side-p unit)
	(check (pos (1+ (x pos)) (1+ (y pos))))
	(check (pos (1- (x pos)) (1- (y pos))))))

  (defun dl (unit pos)
    (declare (dynamic-extent unit pos))
    (if (my-side-p unit)
	(check (pos (1- (x pos)) (1- (y pos))))
	(check (pos (1+ (x pos)) (1+ (y pos))))))

  (defun dr (unit pos)
    (declare (dynamic-extent unit pos))
    (if (my-side-p unit)
	(check (pos (1+ (x pos)) (1- (y pos))))
	(check (pos (1- (x pos)) (1+ (y pos))))))

  (defun placable-p (x y)
    (declare (type int x y))
    (and (<= x 4) (>= x 0) (<= y 4) (>= y 0)
	 (let ((cell (aref *board* x y)))
	   (or (null cell)
	       (= (side cell) +op-side+)))))

  (defun ka-moves (unit pos)
    (declare (dynamic-extent unit pos)
	     (ignorable unit))
    (let ((x (x pos))
	  (y (y pos))
	  (moves '()))
      (declare (int x y))
      (iter (for (the int i) downfrom (1- x))
	    (for (the int j) downfrom (1- y))
	    (while (and (>= i 0) (>= j 0)))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      (iter (for (the int i) from (1+ x))
	    (for (the int j) from (1+ y))
	    (while (and (<= i 4) (<= j 4)))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      (iter (for (the int i) downfrom (1- x))
	    (for (the int j) from (1+ y))
	    (while (and (>= i 0) (<= j 4)))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      (iter (for (the int i) from (1+ x))
	    (for (the int j) downfrom (1- y))
	    (while (and (<= i 4) (>= j 0)))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      moves))

  (defun hi-moves (unit pos)
    (declare (dynamic-extent unit pos)
	     (ignorable unit))
    (let ((x (x pos))
	  (y (y pos))
	  (moves '()))
      (declare (int x y))
      (iter (for (the int i) downfrom (1- x))
	    (with j = y)
	    (while (>= i 0))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      (iter (for (the int i) from (1+ x))
	    (with j = y)
	    (while (<= i 4))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      (iter (with i = x)
	    (for (the int j) downfrom (1- y))
	    (while (>= j 0))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      (iter (with i = x)
	    (for (the int j) from (1+ y))
	    (while (<=  j 4))
	    (progn
	      (let ((cell (aref *board* x y)))
		(if (null cell) 
		    (push (pos i j) moves)
		    (when (= (side cell) +op-side+)
		      (push (pos i j) moves)
		      (finish))))))
      moves))

  (defun ou-moves (unit pos)
    (declare (dynamic-extent unit pos)
	     (ignorable unit))
    (let ((x (x pos))
	  (y (y pos)) 
	  (moves '()))
      (declare (int x y))
      (iter (for (the int i) from (1- x) to (1+ x))
	    (iter (for (the int j) from (1- y) to (1+ y))
		  (when (placable-p i j)
		    (push (pos i j) moves))))
      moves))

  (defun ki-moves (unit pos)
    (declare (dynamic-extent unit pos)
	     (ignorable unit))
    (let ((x (x pos))
	  (y (y pos)) 
	  (moves '()))
      (declare (int x y))
      (iter (for (the int i) from (1- x) to (1+ x))
	    (iter (for (the int j) from y to (1+ y))
		  (when (placable-p i j)
		    (push (pos i j) moves))))
      ;; move back 
      (when (placable-p x (1- y))
	(push (pos x (1- y)) moves))
      moves))

  (defun gi-moves (unit pos)
    (declare (dynamic-extent unit pos)
	     (ignorable unit))
    (let ((x (x pos))
	  (y (y pos)) 
	  (moves '()))
      (declare (int x y))
      (iter (with j = (1+ y))
	    (for (the int i) from (1- x) to (1+ x))
	    (when (placable-p i j)
	      (push (pos i j) moves)))
      (when (placable-p (1- x) (1- y))
	(push (pos (1- x) (1- y)) moves))
      (when (placable-p (1+ x) (1- y))
	(push (pos (1+ x) (1- y)) moves))
      moves))

  (defun fu-moves (unit pos)
    (declare (dynamic-extent unit pos)
	     (ignorable unit))
    (let ((x (x pos))
	  (y (y pos)) 
	  (moves '()))
      (declare (int x y))
      (when (placable-p x (1+ y))
	(push (pos x (1+ y)) moves))
      moves))

  (defalias to-moves ki-moves :space :function)

  (defun um-moves (unit pos)
    (declare (dynamic-extent unit pos))
    (union (ou-moves unit pos)
	   (ka-moves unit pos)))

  (defun ry-moves (unit pos)
    (declare (dynamic-extent unit pos))
    (union (ou-moves unit pos)
	   (hi-moves unit pos)))

  (defalias ng-moves ki-moves :space :function)
  
  ;; end of locally
  )
  

(defun possible-moves (unit pos)
  (declare (dynamic-extent pos unit))
  (case (kind unit)
    (#.+fu+ (fu-moves unit pos))
    (#.+hi+ (hi-moves unit pos))
    (#.+ka+ (ka-moves unit pos))
    (#.+gi+ (gi-moves unit pos))
    (#.+ki+ (ki-moves unit pos))
    (#.+ou+ (ou-moves unit pos))
    (#.+to+ (to-moves unit pos))
    (#.+um+ (um-moves unit pos))
    (#.+ry+ (ry-moves unit pos))
    (#.+ng+ (ng-moves unit pos))))

(defmacro iter-board (code-with-cij)
  `(iter outer (for i below 5)
	 (iter (for j below 5)
	       (for c = (aref *board* i j))
	       ,code-with-cij)))

(defun evaluate ()
  (let* ((positions
	  (iter-board (when (and c (= (side c) +my-side+))
			(in outer (collect (list c (pos i j)))))))
	 (region
	  (mapcan (lambda (e) (possible-moves (first e) (second e))) positions)))
    ))

