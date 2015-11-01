(defun variable-type (var &optional env)
  nil
  #+sbcl (cdr (assoc 'cl:type (nth-value 2 (sb-cltl2:variable-information var env)))))

(defmacro my-psetf (&environment env &rest args)
  (assert (and (> (length args) 0)
               (evenp (length args))))
  (multiple-value-bind (places vals newvars)
      (loop with e = args
            while e
            collect (pop e) into places
            collect (pop e) into vals
            collect (gensym "NEW") into newvars
            finally (return (values places vals newvars)))
    `(let* ,(mapcar (lambda (p v n)
                      (if (and (symbolp p)
                               (variable-type p env))
                          `(,n (the ,(variable-type p env) ,v))
                          `(,n ,v)))
             places vals newvars)
       (setf ,@(mapcan #'list places newvars)))))

(defmacro nlet-tc (name bindings &body body)
  (alexandria:with-unique-names (entry return)
    (let* ((bindings (mapcar #'alexandria:ensure-list bindings))
           (vars (mapcar #'first bindings)))
      `(macrolet ((,name (&rest args)
                    `(progn
                       (my-psetf ,@(mapcan #'list ',vars args))
                       (go ,',entry))))
         (let ,bindings
           (block ,return
             (tagbody
                ,entry
                (return-from ,return
                  (locally ,@body)))))))))

(defmacro nlambda-tc (name args &body body)
  `(labels ((,name ,args
              (nlet-tc ,name ,(mapcar (lambda (a) `(,a ,a)) args)
                       ,@body)))
     #',name))

(defmacro defun-tc (name args &body body)
  `(defun ,name ,args
     (nlet-tc ,name ,(mapcar (lambda (a) `(,a ,a)) args)
              ,@body)))

;;; bug: variable renaming
(defmacro labels-tc (definitions &body body)
  (multiple-value-bind (names argss bodies jump-tags)
      (loop for d in definitions
            for n = (first d)
            for a = (second d)
            for b = (rest (rest d))
            for j = (gensym (symbol-name n))
            collect n into ns
            collect a into as
            collect b into bs
            collect j into js
            finally (return (values ns as bs js)))
    (alexandria:with-unique-names (entry exit name dispatch)
      `(labels ((,entry (,name &key ,@(remove-duplicates (mapcan #'copy-list argss)))
                  (macrolet (,@(mapcar (lambda (name args jump-tag)
                                         `(,name (&rest arrrgs)
                                                 `(progn
                                                    (my-psetf ,@(mapcan #'list ',args arrrgs))
                                                    (go ,',jump-tag))))
                                 names argss jump-tags))
                    (block ,exit
                      (tagbody
                         ,dispatch
                         (case ,name
                           ,@(mapcar (lambda (name jump-tag)
                                       `(,name (go ,jump-tag)))
                              names jump-tags))
                         ,@(mapcan (lambda (jump-tag body)
                                     `(,jump-tag
                                       (return-from ,exit
                                         (locally ,@body))))
                                   jump-tags bodies)))))
                ,@(mapcar (lambda (name args)
                            `(,name ,args
                                    (,entry
                                     ',name
                                     ,@(mapcan (lambda (a)
                                                 `(,(alexandria:make-keyword a) ,a))
                                               args))))
                    names argss))
         (locally ,@body)))))


#|

(defun-tc sum (n acc)
  (declare (fixnum n acc)
           (optimize speed (safety 0)))
  (if (<= n 0)
      acc
      (sum (- n 1) (+ acc n))))

;; benchmark
(defun-tc test (n acc)
  (declare (fixnum n acc)
           (optimize speed (safety 0)))
  (if (<= n 0)
      acc
      (test (the fixnum (1- n))
            (the fixnum (1+ acc)))))

(defun-tc test (n acc)
  (declare (fixnum n acc)
           (optimize speed (safety 0)))
  (if (<= n 0)
      acc
      (test (1- n) (1+ acc))))

(defun test (n acc)
  (declare (fixnum n acc)
           (optimize speed (safety 0)))
  (if (<= n 0)
      acc
      (test (1- n) (1+ acc))))

(defun test (n)
  (declare (fixnum n)
           (optimize speed (safety 0)))
  (labels-tc ((my-oddp (n)
                       (declare (fixnum n))
                       (if (= n 0)
                           nil
                           (my-evenp (the fixnum (1- n)))))
              (my-evenp (m)
                        (declare (fixnum m))
                        (if (= m 0)
                            t
                            (my-oddp (the fixnum (1- m))))))
    (my-evenp n)))

(defun test (n)
  (declare (fixnum n)
           (optimize speed (safety 0)
                     ;;(debug 0)
                     ))
  (labels ((my-oddp (n)
             (declare (fixnum n))
             (if (= n 0)
                 nil
                 (my-evenp (the fixnum (1- n)))))
           (my-evenp (m)
             (declare (fixnum m))
             (if (= m 0)
                 t
                 (my-oddp (the fixnum (1- m))))))
    (my-evenp n)))



|#
