(defun fast-keywords-strip (args)
  (iter (for a in args)
        (unless (eq a '&key)
          (collect (car (ensure-list a))))))

(defmacro defun-fast (name args &body body)
  (with-unique-names (name-fast rest)
    `(progn
       (defun ,name ,args ,@body)
       (defun ,name-fast ,(fast-keywords-strip args) ,@body) 
       (define-compiler-macro ,name (&rest ,rest)
         (destructuring-bind ,args ,rest
           `(,',name-fast ,,@(fast-keywords-strip args))))
       (declaim (inline ,(symbolicate name '-inline)))
       (defun ,(symbolicate name '-inline) ,args ,@body))))

(defun slow (a b &key (c 0) (d 0))
  (declare (type fixnum a b c d))
  (#+sbcl truly-the
   #-sbcl the
   fixnum (+ a b c d)))

(defun-fast fast (a b &key c d)
  (declare (type fixnum a b c d))
  (#+sbcl truly-the
   #-sbcl the
   fixnum (+ a b c d)))

(#+sbcl defglobal
 #-sbcl defvar
 acc 0)

(defmacro bench (&body body)
  `(funcall
    (compile
     nil
     (lambda ()
       ,@body))))

(bench
  (time
   (loop for i of-type fixnum below 1000000000
         do (setf acc (slow 1 2 :d i :c 1000000000)))))

(bench
  (time
   (loop for i of-type fixnum below 1000000000
         do (setf acc (fast 1 2 :d i :c 1000000000)))))

(bench
  (time
   (loop for i of-type fixnum below 1000000000
         do (setf acc (fast-inline 1 2 :d i :c 1000000000)))))
