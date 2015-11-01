(defpackage :hjs.walker
    (:use :cl :alexandria))

(in-package :hjs.walker)

(defun parse (b env)
  (list (car b)
        (sb-cltl2:parse-macro (car b) (cadr b) (cddr b) env)))

(defun with-env (env form bind fn body)
  (funcall fn body
           (ecase form
             (macrolet
                 (sb-cltl2:augment-environment env :macro (mapcar (lambda (b) (parse b env)) bind)))
             ((flet labels)
                (sb-cltl2:augment-environment env :function (mapcar #'car bind)))
             ((symbol-macro symbol-macrolet)
                ...))))


