(defun start-tour (position)
  (let ((board (make-array '(8 8))))
    (move position board 1)))

(defun move (position board n)
  (setf (aref board (car position) (cadr position)) n)
;;  (print-board board)
;;  (print (evaluate-possible-moves position board))
;;  (format t "~%")
  (cond ((= n 64) (print-board board))
	((null (possible-moves position board)) 'failed-dead-end)
	((< n 64) (move (best-move position board) board (+ n 1)))))


(defun best-move (position board)
  (car (evaluate-possible-moves position board)))

(defun evaluate-possible-moves (position board)
;;  (print position)
;;  (format t "~%")
  (sort (mapcar #'(lambda (x) (append x (list (length (possible-moves x board)))))
	        (possible-moves position board))
	#'(lambda (triple-a triple-b) (< (caddr triple-a) (caddr triple-b)))))

(defun print-board (board)
  (do ((x 0 (+ x 1)))
      ((= x 8))
       (do ((y 0 (+ y 1)))
	   ((= y 8))
         (format t "~2D " (aref board x y)))
       (format t "~%"))
  (format t "~%"))

(defun on-board-p (position)
   (and (>= (car position) 0)
	(<= (car position) 7)
	(>= (cadr position) 0)
	(<= (cadr position) 7)))

(defun visited-p (position board)
  (> (aref board (car position) (cadr position)) 0))

(defun possible-moves (position board)
  (remove-if #'(lambda (x) (visited-p x board)) (moves position)))


(defun moves (position)
  (let ((x (car position))
	(y (cadr position)))
    (remove-if-not #'on-board-p
  	   	   `((,(+ x 1) ,(+ y 2))
		     (,(+ x 1) ,(- y 2))
                     (,(- x 1) ,(+ y 2))
                     (,(- x 1) ,(- y 2))
	             (,(+ x 2) ,(+ y 1))
	             (,(+ x 2) ,(- y 1))
	             (,(- x 2) ,(+ y 1))
	             (,(- x 2) ,(- y 1))))))
