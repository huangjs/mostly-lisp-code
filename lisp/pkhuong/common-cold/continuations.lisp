(cl:in-package "SERIALISABLE-CONTINUATIONS")

(defclass dynamic-binding ()
  ((var :reader var-of :initarg :var)
   (val :reader val-of :initarg :val)))

(defun dyn-bind (var val)
  (make-instance 'dynamic-binding
                 :var var
                 :val val))

(defun s:d (var val)
  (dyn-bind var val))

(defmethod print-object ((obj dynamic-binding) stream)
  (let ((*package* (find-package "KEYWORD")))
    (format stream "#.~S" `(s:d ',(var-of obj)
                                ',(val-of obj)))))

(defclass catch-frame ()
  ((tag :reader tag-of :initarg :tag)))

(defun catch-frame (tag)
  (make-instance 'catch-frame
                 :tag tag))

(defun s:c (tag)
  (catch-frame tag))

(defmethod print-object ((obj catch-frame) stream)
  (let ((*package* (find-package "KEYWORD")))
    (format stream "#.~S" `(s:c ',(tag-of obj)))))

(defmacro bind ((var val) expr)
  (let ((blk (gensym "BLOCK"))
        (fn  (gensym "RECV")))
    `(flet ((,fn (,var)
              (declare (ignorable ,var))
              ,expr))
       (,fn (block ,blk
              (let ((cont (cons (sfunction ,fn)
                                (catch 'capture
                                  (return-from ,blk ,val)))))
                (throw 'capture
                  cont)))))))

(defmacro dbind ((var val) expr)
  (let ((blk (gensym "BLOCK"))
        (fn  (gensym "RECV")))
    `(flet ((,fn (,var)
              (declare (special ,var))
              (block ,blk
                (let ((cont (catch 'capture
                              (return-from ,blk ,expr))))
                  (throw 'capture
                    (cons (dyn-bind ',var ,var)
                          cont))))))
       (,fn (block ,blk
              (let ((cont (cons (sfunction ,fn)
                                 (catch 'capture
                                   (return-from ,blk ,val)))))
                (throw 'capture
                  cont)))))))

(defmacro mprogn (expr &rest exprs)
  (if (null exprs)
      expr
      (let ((_ (gensym "_")))
        `(bind (,_ ,expr)
               (mprogn ,@exprs)))))

(defmacro mlet* ((&rest bindings) &body exprs)
  (if (null bindings)
      `(mprogn ,@exprs)
      `(bind ,(first bindings)
             (mlet* ,(rest bindings)
                    ,@exprs))))

(defmacro mdlet* ((&rest bindings) &body exprs)
  (if (null bindings)
      `(mprogn ,@exprs)
      `(dbind ,(first bindings)
              (mdlet* ,(rest bindings)
                      ,@exprs))))

(defmacro mlet ((&rest bindings) &body exprs)
  (if (null (rest bindings))
      `(mlet* ,bindings ,@exprs)
      (let ((bindings (mapcar (lambda (binding)
                                (cons (gensym (symbol-name (first binding)))
                                      binding))
                              bindings)))
        `(mlet* ,(mapcar (lambda (binding)
                           (destructuring-bind (tmp var val)
                               binding
                             (declare (ignore var))
                             (list tmp val)))
                         bindings)
           (mlet* ,(mapcar (lambda (binding)
                             (destructuring-bind (tmp var val)
                                 binding
                               (declare (ignore val))
                               (list var tmp)))
                           bindings)
             ,@exprs)))))

(defmacro mdlet ((&rest bindings) &body exprs)
  (if (null (rest bindings))
      `(mdlet* ,bindings ,@exprs)
      (let ((bindings (mapcar (lambda (binding)
                                (cons (gensym (symbol-name (first binding)))
                                      binding))
                              bindings)))
        `(mlet* ,(mapcar (lambda (binding)
                           (destructuring-bind (tmp var val)
                               binding
                             (declare (ignore var))
                             (list tmp val)))
                         bindings)
           (mdlet* ,(mapcar (lambda (binding)
                              (destructuring-bind (tmp var val)
                                  binding
                                (declare (ignore val))
                                (list var tmp)))
                            bindings)
             ,@exprs)))))

(defmacro mcatch (tag &body body)
  (let ((tag-var (gensym "TAG-VAR"))
        (blk     (gensym "BLK")))
    `(bind (,tag-var ,tag)
           (block ,blk
             (let ((cont (catch 'capture
                           (return-from ,blk
                             (mprogn ,@body)))))
               (throw 'capture
                 (cons (catch-frame ,tag-var)
                       cont)))))))

(defparameter *return-tags* '())

(defmacro mblock (tag &body exprs)
  (assert (symbolp tag))
  (let ((var (gensym "BLOCK-VAR")))
    `(block ,tag
       (let ((,var ',var))
         (compiler-let ((*return-tags* ',(acons tag var
                                                *return-tags*)))
           (mprogn ,@exprs))))))

(defmacro mreturn-from (tag &optional (value nil valuep))
  (let ((var (cdr (assoc tag *return-tags*))))
    (if var
        `(throw ,var ,value)
        `(return-from ,tag ,@ (when valuep
                                (list value))))))

(defmacro mreturn (&optional (value nil valuep))
  `(mreturn-from nil ,@ (when valuep
                          (list value))))

(defun invoke-cont (cont &key thunk value)
  (labels
      ((inner (cont)
         (if (null cont)
             (if thunk
                 (funcall thunk)
                 value)
             (destructuring-bind (frame . contp)
                 cont
               (etypecase frame
                 (dynamic-binding
                  (progv (list (var-of frame))
                      (list (val-of frame))
                    (block nil
                      (let ((cont
                             (catch 'capture
                               (return (inner contp)))))
                        (throw 'capture
                          (cons (dyn-bind (var-of frame)
                                          (symbol-value
                                           (var-of frame)))
                                cont))))))
                 (catch-frame
                  (catch (tag-of frame)
                    (block nil
                      (let ((cont
                             (catch 'capture
                               (return (inner contp)))))
                        (throw 'capture
                          (cons frame
                                cont))))))
                 (serialisable-closure
                  (funcall
                   frame
                   (block nil
                     (let ((cont
                            (catch 'capture
                              (return (inner contp)))))
                       (throw 'capture (cons frame
                                             cont)))))))))))
    (inner cont)))

(defun prune-redundant-frames (cont)
  "Removes redundant frames for dynamic
stuff (bindings, catch frames) -> ensure TCO"
  (let ((new-cont '()))
    (dolist (frame (reverse cont) new-cont) ; cont accumulate bottom first
      (if (null new-cont)
          (push frame new-cont)
          (etypecase frame
            (serialisable-closure
             (push frame new-cont))
            (dynamic-binding
             (unless (and (typep (first new-cont)  ;if redundant
                                 'dynamic-binding) 
                          (eq (var-of (first new-cont))    ; (tail calls)
                              (var-of frame)))
               (push frame new-cont)))             ; only keep topmost
            (catch-frame
             (unless (and (typep (first new-cont)
                                 'catch-frame)
                          (eq (tag-of frame)
                              (tag-of (first new-cont))))
               (push frame new-cont))))))))
