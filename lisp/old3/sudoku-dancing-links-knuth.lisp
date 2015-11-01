(defpackage :sudoku
    (:use :common-lisp))
  
(in-package :sudoku)

(defclass matrix-element ()
  ((up :accessor up :initarg :up)
   (down :accessor down :initarg :down)
   (left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))

(defmethod initialize-instance :after ((me matrix-element) &rest initargs &key &allow-other-keys)
  (setf (up me) me)
  (setf (down me) me)
  (setf (left me) me)
  (setf (right me) me))

(defclass column-header (matrix-element)
  ((size :accessor size :initform 0)
   (name :accessor name :initarg :name)))

(defclass column-element (matrix-element)
  ((column-header :accessor column-header :initarg :column-header)
   (triplet :accessor triplet :initarg :triplet)))

(defun insert-above (element new)
  "Insert a new element above the existing element. And increase size of column by 1"
  (setf (up new) (up element)
        (down new) element
        (down (up element)) new
        (up element) new)
  (incf (size (column-header new)))
  element)

(defun insert-rightof (element new)
  (setf (right new) (right element)
        (left new) element
        (left (right element)) new
        (right element) new)
  element)

(defmethod initialize-instance :after ((me column-element) &rest initargs &key &allow-other-keys)
  (if (column-header me)
    (insert-above (column-header me) me)))

(defun remove-horizontal (element)
  (setf (left (right element)) (left element)
        (right (left element)) (right element))
  element)

(defun replace-horizontal (element)
  (setf (left (right element)) element
        (right (left element)) element)
  element)

(defun remove-vertical (element)
  (setf (up (down element)) (up element)
        (down (up element)) (down element))
  (decf (size (column-header element)))
  element)

(defun replace-vertical (element)
  (setf (up (down element)) element
        (down (up element)) element)
  (incf (size (column-header element)))  
  element)

(defmacro rcv-loop (&rest body)
  `(loop for r from 0 to 8
         do (loop for c from 0 to 8
                  do (loop for v from 0 to 8
                           do ,@body))))


(defun add-constraint-headers (matrix elem1 elem2)
  (loop for i from 0 to 8
        do (loop for j from 0 to 8
            do (insert-rightof (left matrix) 
                               (make-instance 'column-header
                                              :name (format nil "~a: ~a/~a: ~a" elem1 (+ 1 i) elem2 (+ 1 j)))))))

(defun sudoinit ()
  "returns an initial matrix representing an empty board."
  (let ((matrix (make-instance 'matrix-element))
        (col-array (make-array (* 4 9 9)))
        (row-array (make-array (* 9 9 9))))
    (add-constraint-headers matrix "Row" "Col")
    (add-constraint-headers matrix "Row" "Number")      
    (add-constraint-headers matrix "Col" "Number")      
    (add-constraint-headers matrix "Block" "Number")
    (loop with foo = (right matrix)
          for i from 0
          until (eql foo matrix)
          do (setf (aref col-array i) foo
                   foo (right foo)))
    (rcv-loop
     (let* ((triplet (list r c v))
            (r-c (make-instance 'column-element
                                :column-header (aref col-array (+ (* 9 r)
                                                                  c))
                                :triplet triplet))
            (r-v (make-instance 'column-element
                                :column-header (aref col-array (+ (* 9 9)
                                                                  (* 9 r)
                                                                  v))
                                :triplet triplet))
            (c-v (make-instance 'column-element
                                :column-header (aref col-array (+ (* 2 9 9)
                                                                  (* 9 c)
                                                                  v))
                                :triplet triplet))
            (b-v (make-instance 'column-element
                                :column-header (aref col-array (+ (* 3 9 9)
                                                                  (* 9 (+ (* 3 (floor r 3))
                                                                          (floor c 3)))
                                                                  v))
                                :triplet triplet)))
       (insert-rightof r-c r-v)
       (insert-rightof r-v c-v)
       (insert-rightof c-v b-v)
       (setf (aref row-array (+ (* 9 (+ (* 9 r) c)) v)) r-c)))
    (values matrix row-array)))

(defun cover-col (col-header)
  (do ((elem (down col-header) (down elem)))
      ((eql elem col-header) col-header)
    (do ((row-elem (right elem) (right row-elem)))
        ((eql row-elem elem) row-elem)
      (remove-vertical row-elem)))
  (remove-horizontal col-header))

(defun uncover-col (col-header)
  (do ((elem (up col-header) (up elem)))
      ((eql elem col-header) col-header)
    (do ((row-elem (right elem) (right row-elem)))
        ((eql row-elem elem) row-elem)
      (replace-vertical row-elem)))
  (replace-horizontal col-header))

(defun cover-row (elem)
  (do ((column-iter (right elem) (right column-iter)))
      ((eql elem column-iter))
    (cover-col (column-header column-iter))))

(defun uncover-row (elem)
  (do ((column-iter (left elem) (left column-iter)))
      ((eql elem column-iter))
    (uncover-col (column-header column-iter))))

(defun try-elem (elem array solution)
  (push elem solution)
  (cover-row elem)
  (let ((foo (find-solution array solution)))
    (uncover-row elem)
    (pop solution)
    foo))

(defun find-smallest-col (array)
  (do ((head-iter (right array) (right head-iter))
       (cur-min (right array) (if (< (size head-iter) (size cur-min))
                                head-iter
                                cur-min)))
      ((or (= 1 (size cur-min))
           (eql head-iter array)) cur-min)))

(defun find-solution (array solution)
  (if (eql (left array) array)
    (progn
      (print-solution solution)
      t)
    (let ((col-header (find-smallest-col array)))
      (prog2
          (cover-col col-header)
          (do ((row-elem (down col-header) (down row-elem)))      
              ((or (eql row-elem col-header)
                   (try-elem row-elem array solution))
               (not (eql row-elem col-header))))
        (uncover-col col-header)))))

(let ((solc 0))
  (defun print-solution (solution)
    (format t "  ~a  " (incf solc))
    (loop with arr = (make-array '(9 9))
          for s in solution
          do (destructuring-bind (i j d) (triplet s)
               (setf (aref arr i j) (+ 1 d)))
          finally (loop for i from 0 to 8
                        do (format t "~{~A~}"
                                   (loop for j from 0 to 8
                                         collect (aref arr i j))))))
  
  (defun reset-count ()
    (setf solc 0)))

(defun read-starting-position (pos-string)
  (with-input-from-string (s pos-string)
    (remove-if 'null
               (apply 'append
                      (loop for r from 0 to 8
                            collect (loop for c from 0 to 8
                                          collect (let ((v (- (char-code (read-char s))
                                                              (char-code #\0))))
                                                    (when (> v 0)
                                                      (list r c (1- v))))))))))

(defun solve-file (filename)
  (reset-count)
  (multiple-value-bind (matrix rowarr) (sudoinit)
    (with-open-file (s filename)
      (do ((string (read-line s nil 'eof) (read-line s nil 'eof)))
          ((eql string 'eof) nil)
	(format t "~%~a" string)
        (solve string matrix rowarr)))))

(defun solve (pos-string &optional matrix rowarr)
  (unless (and matrix rowarr)
    (multiple-value-bind (foo bar) (sudoinit)
      (setf matrix foo)
      (setf rowarr bar)))
  (let ((solution ()))
    (dolist (triplet (read-starting-position pos-string))
      (destructuring-bind (i j d) triplet
        (push (aref rowarr (+ (* 9 (+ (* 9 i) j)) d)) solution)
        (cover-col (column-header (aref rowarr (+ (* 9 (+ (* 9 i) j)) d))))
        (cover-row (aref rowarr (+ (* 9 (+ (* 9 i) j)) d)))))
    (find-solution matrix solution)
    (do ((elem (pop solution) (pop solution)))
        ((null elem) matrix)
      (uncover-row elem)
      (uncover-col (column-header elem)))))

