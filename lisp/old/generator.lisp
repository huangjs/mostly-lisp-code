(defpackage #:generators
  (:use #:cl #:arnesi))

(in-package #:generators)

(defstruct generator iterator)

(defmacro defgenerator (name arguments &body body)
  (let ((point (gensym))
        (current (gensym))
        (iterate (gensym))
        (invalidate (gensym))
        (invalid (gensym)))
    `(defun ,name ,arguments
       (let (,point ,current)
         (labels ((,iterate ()
                    "Return the next value from the generator."
                    (cond ((eq ,current ',invalid)
                           (cons nil nil))
                          (t
                           (let ((current ,current))
                             (funcall ,point nil)
                             (cons current t)))))
                  (,invalidate ()
                    (setf ,current ',invalid)))
           (with-call/cc
             (labels ((yield (value)
                        "If value is itself a generator
                        then yield each of its values."
                        (cond ((generator-p value)
                               (do-generator (item value)
                                 (yield item)))
                              (t
                               (setf ,current value)
                               (let/cc k
                                 (setf ,point k))))))
               ,@body
               (,invalidate)))
           (make-generator :iterator #',iterate))))))

(defmacro do-generator ((var generator) &body body)
  (let ((gen (gensym))
        (not-finished-p (gensym)))
    `(let ((,gen ,generator))
       (loop for (,var . ,not-finished-p) = (funcall (generator-iterator ,gen))
          while ,not-finished-p
          do (progn ,@body)))))

(defun mapc-generator (function generator)
  (do-generator (item generator)
    (funcall function item))
  (values))

(defun mapcar-generator (function generator)
  (let ((result ()))
    (do-generator (item generator)
      (push (funcall function item) result))
    (nreverse result)))
