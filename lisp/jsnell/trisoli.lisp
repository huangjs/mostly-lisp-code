(declaim (optimize speed))

(deftype board () '(simple-array (unsigned-byte 8) (3 3)))

#|
x-x-x
|\ /|
x x x
|/ \|
x-x-x
|#

(defconstant +empty+ 0)
(defconstant +white+ 1)
(defconstant +blue+ 2)
(defconstant +red+ 3)
(declaim (type (simple-array list (3 3)) +possible-directions+))
(defvar +possible-directions+
  #2A(( ((1 0) (0 1) (1 1)) ((-1 0) (1 0)) ((-1 0) (-1 1) (0 1)) )
      ( ((0 -1) (0 1)) ((-1 -1) (1 -1) (-1 1) (1 1)) ((0 -1) (0 1)) )
      ( ((0 -1) (1 -1) (1 0)) ((-1 0) (1 0)) ((-1 0) (-1 -1) (0 -1)) )))

(defun make-start-board ()
  (make-array '(3 3) :element-type '(unsigned-byte 8) :initial-contents
              `((,+white+ ,+empty+ ,+blue+)
                (,+white+ ,+red+ ,+blue+)
                (,+white+ ,+empty+ ,+blue+))))

(defvar +end-board+
  #.(make-array '(3 3) :element-type '(unsigned-byte 8) :initial-contents
                `((,+empty+ ,+blue+ ,+white+)
                  (,+blue+ ,+red+ ,+white+)
                  (,+blue+ ,+empty+ ,+white+))))

(defun get-possible-targets (board x y)
  (declare (type board board))
  (unless (= +empty+ (aref board y x))
    (loop for (xd yd) fixnum in (aref +possible-directions+ y x)
          for nx = (+ x xd)
          for ny = (+ y yd)
          when (= +empty+ (aref board ny nx))
          collect (cons nx ny))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun board-value (board)
    (declare (type board board))
    (loop for x from 0 below 3
          with result fixnum = 0 and mult fixnum = 1
          finally (return result) do
          (loop for y from 0 below 3 do
                (incf result (* mult (aref board y x)))
                (setf mult (* 4 mult))))))

(defvar +end-board-value+ (board-value +end-board+))

(defun move (board x-from y-from x-to y-to)
  (setf (aref board y-to x-to) (aref board y-from x-from)
        (aref board y-from x-from) +empty+))

(defun print-solution (path)
  (format t "path length: ~a, path: " (length path))
  (loop for ((x0 . y0) (x1 . y1)) in path do
        (format t "~a~a-~a~a " x0 y0 x1 y1))
  (terpri))

(defun copy-board (board)
  (declare (type board board))
  (let ((a (make-array '(3 3) :element-type '(unsigned-byte 8))))
    (dotimes (r 3)
      (dotimes (c 3)
        (setf (aref a r c) (aref board r c))))
    a))

(defun print-board (board)
  (loop for y from 0 below 3 do
        (loop for x from 0 below 3 do
              (format t "~a" (aref board y x)))
        (terpri))
  (terpri))

(defun find-path-impl (visited move-big level paths)
  (declare (type simple-bit-vector visited))
  (when (> (length level) 0)
    (let (new-level new-paths)
      (loop for board of-type board in level
            for path list in paths do
            (loop for x from 0 below 3 do
                  (loop for y from 0 below 3 do
                        (when (eql move-big (= +red+ (aref board y x)))
                          (loop for (xt . yt) in (get-possible-targets board x y) do
                                (let ((new-board (copy-board board))
                                      (new-path path))
                                  (move new-board x y xt yt)
                                  (push (list (cons x y) (cons xt yt)) new-path)
                                  (let ((value (board-value new-board)))
                                    (when (= value +end-board-value+)
                                      (return-from find-path-impl (nreverse new-path)))
                                    (when move-big (incf value 262144))
                                    (when (or nil (= 0 (bit visited value)))
                                      (setf (bit visited value) 1)
                                      (push new-path new-paths)
                                      (push new-board new-level)))))))))
      (find-path-impl visited (not move-big) new-level new-paths))))

(defun find-path ()
  (let ((visited (make-array (* 2 262144)
                             :element-type 'bit :initial-element 0))
        (board (make-start-board))
        (level '()))
    (push board level)
    (let ((result (find-path-impl visited nil level '(()))))
      (if result
          (print-solution result)
        (format t "no solution found")))))

