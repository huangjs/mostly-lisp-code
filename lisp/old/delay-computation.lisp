;;; primitive solution, not good enough
;;; use syntax used in SCHEME: delay, force.
(defstruct delay (value nil) (function nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :function #'(lambda () . ,body)))

(defun force (x)
  "Find the value of x, by computing if it is a delay."
  (if (not (delay-p x))
      x
      (progn
        (when (delay-function x)
          (setf (delay-value x)
                (funcall (delay-function x)))
          (setf (delay-function x) nil))
        (delay-value x))))

;;; example
;;; (setf x (list (print 1) (delay (print 2))))
;;; (force (second x))

;;; infinite stream (also called pipe)
(defmacro make-pipe (head tail)
  "Create a pipe by evaluating head and delaying tail."
  `(cons ,head (delay ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe)
  (first pipe))

(defun tail (pipe)
  (force (rest pipe)))

;;; helper macro
(defmacro defalias (new-function-name old-function-name)
    `(defun ,new-function-name (&rest args)
        (apply #',old-function-name args)))

;;; define alias
(defalias pipe-first head)
(defalias pipe-rest tail)
(defalias pipe-car head)
(defalias pipe-cdr tail)
(defalias stream-car head)
(defalias stream-cdr tail)

(defun pipe-elt (pipe n)
  "The nth element of a pipe, 0-based"
  (if (= n 0)
      (head pipe)
      (pipe-elt (tail pipe) (- n 1))))

;;; examples
(defun integers (&optional (start 0) end)
  "A pipe of integers from START to END.
If END is nil, this is an infinite pipe."
  (if (or (null end) (<= start end))
      (make-pipe start (integers (+ start 1) end))
      nil))


;;; the original one waste a lot of storage space.
;;; TAIL or PIPE-ELT must traverse the structures.
;;; an IMPROVED version
;;; use closures instead of delay structures.
;;; representation for pipes is now (value . closure)
;;; close to the SCHEME implementation
(defun make-pipe (head tail)
  "Create a pipe by evaluating head and delaying tail."
  `(cons ,head #'(lambda () ,tail)))

;;; the rest can be only one of: nil, a list, or a delayed value (closure).
;;; compiled closures are atoms, so it's easy.
(defun tail (pipe)
  "Return tail of pipe or list, and destructively update
the tail if it is a function."
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))

;;; everything else remains the same
