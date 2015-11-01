(defpackage :hjs.scheme
    (:use :cl :alexandria))

(in-package :hjs.scheme)

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

