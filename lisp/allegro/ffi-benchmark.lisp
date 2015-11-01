(declaim (inline d-exp my-d-exp))

#+allegro
(ff:def-foreign-call (d-exp "exp") ((arg :double))
  :returning :double
  :arg-checking nil
  :call-direct t)

#+sbcl
(require :sb-sprof)
#+sbcl
(cffi:defcfun (d-exp "exp") :double
  (x :double))

(defun foo ()
  (declare (optimize speed (safety 0) (debug 0)))
  (loop for i of-type fixnum below 100000000
        for d of-type double-float = (d-exp 3d0)
        do (progn)))

(defun bar (x)
  (declare (optimize speed (safety 0) (debug 0))
           (type double-float x))
  (loop for i of-type fixnum below 100000000
        for d of-type double-float = (* x 3d0)
        do (progn (setf x d))))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'my-d-exp 'sys::immed-args-call)
	'((double-float) double-float)))
(defun my-d-exp (x)
  (declare (optimize speed (safety 0))
           (type double-float x))
  (d-exp x))

(defun foo2 ()
  (declare (optimize speed (safety 0) (debug 0)))
  (loop for i of-type fixnum below 100000000
        for d of-type double-float = (my-d-exp 3d0)
        do (progn)))

#| hack

(pushnew 'foo comp::*hack-compiler-output*)
(pushnew 'my-d-exp comp::*hack-compiler-output*)

(setf comp::*assemble-function-body*
      '((foo . "/home/huang/programming/lisp/allegro/foo-hack.s")
        (my-d-exp . "/home/huang/programming/lisp/allegro/my-d-exp-hack.s")))

|#


#|

(handler-bind ((simple-break
                (lambda (c)
                  (let ((source (with-open-file (f "hackit.s")
                                  (loop for ins = (read f nil nil)
                                        while ins collect ins))))
                    (with-open-file (f "hackit.s" :direction :output :if-exists :supersede)
                      (write
                       (loop for cursor = source then (cdr cursor)
                             while cursor
                             for ins = (car cursor)
                             if (or (and (eq 'push.q (car ins)) 
                                         (eq 'popf.l (car (second cursor))))
                                    (eq 'popf.l (car ins))
                                    (eq 'ldmxcsr (car ins)))
                               do (print ins)
                             else
                               collect ins
                             end)
                       :stream f)
                      (finish-output f))
                    (invoke-restart 'continue)))))
  (compile 'my-d-exp))

|#
