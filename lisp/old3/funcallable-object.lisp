#|
Funcallable objects come in handy when you want (or need) to use a function as the interface to an object or a collection of objects. I generally use them when I need to define a mapping that changes over time and holding the mapping outside of the function would be cumbersome. They're a handy tool to have around.
|#


;;; (asdf:operate 'asdf:load-op :closer-mop)

(defclass setl-map ()
  ((pairs :initarg :pairs :initform nil :accessor setl-map-pairs))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance setl-map) &rest initargs)
  (declare (ignore initargs))
  (sb-mop:set-funcallable-instance-function
   instance
   #'(lambda (x) (rest (assoc x (setl-map-pairs instance))))))


;;; example
(defun f () nil)

(setf (symbol-function 'f)
      (make-instance 'setl-map :pairs '((1 . 100) (2 . 200))))

(defun (setf f) (result arg)
  (let ((setl-map (symbol-function 'f)))
    (push (cons arg result) (setl-map-pairs setl-map))
    result))

(type-of #'f)
(f 1)
(f 2)
(f 3)
(setf (f 3) 300)
(f 3)

