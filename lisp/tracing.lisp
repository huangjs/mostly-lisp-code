(in-package :cl-user)

(defun find-all-functions-in-package (&optional (package *package*))
  (let ((p (find-package package))
        (result '()))
    (with-package-iterator (next-symbol (list p) :internal :external)
      (loop  
        (multiple-value-bind (more? symbol) (next-symbol)
          (if more? 
              (when (and (eq (symbol-package symbol) p)
                         (ignore-errors (symbol-function symbol))
                         (not (macro-function symbol))
                         (functionp (symbol-function symbol)))
                (push symbol result))
              (return)))))
    (nreverse result)))

(defmacro trace-all-functions-in-package (&optional (package *package*))
  `(trace ,@(find-all-functions-in-package package)))


(defmacro trace-all-functions-in-packages (&rest packages)
  `(progn
     ,@(loop for package in packages
             when (not (find-package package))
               do (error "Cannot find package: ~a" package)
             collect `(trace ,@(find-all-functions-in-package package)))))

