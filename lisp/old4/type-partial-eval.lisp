;;; control structure optimizaiton example from jsnell
(sb-c:defknown call-with-stupid-loop (t t t function) *)

(defun call-with-stupid-loop (x var body fun)
  (print 'generic)
  (etypecase x
    (fixnum (dotimes (i x)
              (funcall fun i)))
    (list (dolist (i x)
            (funcall fun i)))))

(sb-c:deftransform call-with-stupid-loop ((x var body function)
                                          (fixnum t t t))
  `(dotimes (,(sb-c::lvar-value var) x)
     ,@(sb-c::lvar-value body)))

(sb-c:deftransform call-with-stupid-loop ((x var body function)
                                          (list t t t))
  `(dolist (,(sb-c::lvar-value var) x)
     ,@(sb-c::lvar-value body)))

(defmacro stupid-loop ((var value) &body body)
  `(call-with-stupid-loop ,value ',var ',body
                          (lambda (,var)
                            ,@body)))

(defun test (y)
  ;; Statically expands to a DOTIMES
  (stupid-loop (x 2)
    (print x))
  ;; Statically expands to a DOLIST
  (stupid-loop (x '(a b c))
    (print x))
  ;; Expands to a call to the generic case
  (stupid-loop (x y)
    (print x))
  (car y)
  ;; Statically expands to a DOLIST
  (stupid-loop (x y)
    (print x))
  nil)
