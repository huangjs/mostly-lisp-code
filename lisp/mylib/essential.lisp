(defconstant +infinity+ most-positive-long-float)

(defun true (&rest ignore)
  "Always return true. Useful for comparing structures instead of contents (use #'eql)."
  t)

;;; with-gensyms
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))

;;; with-unique-names
(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
                     (destructuring-bind (var prefix)
						 (if (consp binding) binding (list binding binding))
                       `(,var (gensym ,(string prefix)))))
                 bindings)
	 ,@body))

;;; alias
;;; the benefit of alias is that the document is also associated to new function
(defmacro alias (new-fn old-fn)
  "Set alias of an old function/macro to a new function/macro.
e.g., (alias 'filter 'remove-if-not)"
  `(setf (symbol-function ',new-fn) (symbol-function ',old-fn)))

(alias with-unique-names with-gensyms)

;;; while control structure
(defmacro while (test &rest body)
  "Repeat body while test is true."
  `(loop (unless ,test (return nil))
    ,@body))

;;; Macro TIMES for benchmark
;;; usage: (times 5000 <body>)
;;; means: do 5000 times of <body> and calculate the total time.
(defmacro times (num &body body)
  (with-gensyms (i)
    `(time
      (dotimes (,i ,num)
        ,@body))))

;;; higher order function combination
(defun compose (f g)
  "Return the function that computes (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

;;; Create symbols
(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol."
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol."
  (make-symbol (format nil "~{~a~}" args)))


;;; Delayed computation:
(defstruct delay value (computed? nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :value #'(lambda () . ,body)))

(defun force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
             (setf (delay-computed? delay) t))))

;;; either, random choice
(defmacro either (form1 form2)
  "Choose form1 or form2 randomly."
  ;;(random 2) returns 0 or 1
  `(if (zerop (random 2)) ,form1 ,form2))

