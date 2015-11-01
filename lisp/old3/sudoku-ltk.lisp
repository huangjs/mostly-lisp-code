(defpackage :sudoku-ltk
  (:use :common-lisp :ltk)
  (:export :sudoku))

(in-package :sudoku-ltk)

(defparameter *LB-VALUES* '(" " "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defparameter *G-CONVERT* #(0 1 2 4 5 6 8 9 10))

(defun clear-numbers (me-numbers)
  (dotimes (row 9)
    (dotimes (col 9)
      (setf (text (aref me-numbers row col)) " ")
      (configure (aref me-numbers row col) :foreground :blue))))

(defun solve-numbers (me-numbers)
  (let ((input-grid (make-array '(9 9)))
        (output-grid (make-array '(9 9))))
    (dotimes (row 9)
      (dotimes (column 9)
        (if (string= (text (aref me-numbers row column)) " ")
          (setf (aref input-grid row column) 0)
          (setf (aref input-grid row column) (parse-integer (text (aref me-numbers row column)))))))
    (setf output-grid (sudoku-screamer:sudoku-solve input-grid))

    (dotimes (row 9)
      (dotimes (column 9)
        (when (string-equal (text (aref me-numbers row column)) " ")
          (setf (text (aref me-numbers row column)) (format nil "~a" (aref output-grid row column)))
          (configure (aref me-numbers row column) :foreground :black))))))


(defun sudoku ()
  "Put up a Sudoku grid, allowing the user to fill in numbers and solve.
   solver-fn should be a function that takes a 9 x 9 array, with 0 for blanks,
   and returns a 9 x 9 array."
  (with-ltk ()
            (let* ((me-numbers (make-array '(9 9)))
                   (f (make-instance 'frame))
                   (numbers (make-instance 'frame :master f :background :grey))
                   (button-holder (make-instance 'frame
                                      :master f))
                   (clear-btn (make-instance 'button
                                       :master button-holder
                                       :text "Clear"
                                       :command #'(lambda () (clear-numbers me-numbers))))
                   (solve-btn (make-instance 'button
                                       :master button-holder
                                       :text "Solve"
                                       :command #'(lambda () (solve-numbers me-numbers))))
                   (bumper-1 (make-instance 'canvas :master numbers :width 1 :height 1))
                   (bumper-2 (make-instance 'canvas :master numbers :width 1 :height 1))
                   (bumper-3 (make-instance 'canvas :master numbers :width 1 :height 1))
                   (bumper-4 (make-instance 'canvas :master numbers :width 1 :height 1)))
              (pack f)
              (pack numbers :fill :both)
              (dotimes (row 9)
                (dotimes (col 9)
                  (setf (aref me-numbers row col) 
                        (make-instance 'ltk-mw:menu-entry :master numbers :content *LB-VALUES* :width 2 :foreground :blue :text " "))
                  (grid (aref me-numbers row col) (aref *g-convert* row)  (aref *g-convert* col))))
              (grid bumper-1 3 3)
              (grid bumper-2 3 7)
              (grid bumper-3 7 3)
              (grid bumper-4 7 7)

              (pack button-holder :side :bottom)
              (pack clear-btn :side :left)
              (pack solve-btn :side :left)
              (configure f :borderwidth 3)
              (configure f :relief :sunken))))
