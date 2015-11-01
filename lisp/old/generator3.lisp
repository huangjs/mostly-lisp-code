

;;;I start out with a continuation datatype: 

(defpackage :generator)


(in-package :generator)


(defclass continuation () 
  ((func-object :initarg :func-object :reader func-object))) 

;;((A -> Answer) -> Answer) -> (continuation-of A) -- a funcallable tag 
(defun continuation (fun) 
  (make-instance 'continuation :func-object fun)) 

(defmethod unit ((type (eql 'continuation)) val) 
  (continuation #'(lambda (current) (funcall current val)))) 

(defmethod bind ((monad-val continuation) next) 
  (continuation #'(lambda (current) 
                    (funcall (func-object monad-val) 
                             (lambda (val) 
                               (funcall (func-object (funcall next val)) 
                                        current)))))) 

(defmethod run ((monad-val continuation)) 
  (funcall (func-object monad-val) #'identity)) 

;;;((A -> continuation) -> continuation) -> continuation 
(defun call/cc (entry-point) 
  (continuation #'(lambda (current) 
                    (flet ((escape (val) 
                             ;;compare with unit 
                             (continuation #'(lambda (final) 
                                               (declare (ignore final)) 
                                               (funcall current val))))) 
                      ;;compare with bind 
                      (funcall (func-object (funcall entry-point #'escape)) 
                               current))))) 

;;;add a helper macro 

;;;just like its says: sequential evaluation threaded with binds 
(defmacro monad-progn (&body forms) 
  (labels ((make-bind-seq (forms) 
             (cond ((null forms) 
                    nil) 
                   ((null (cdr forms)) 
                    (car forms)) 
                   (t (let ((_ (gensym))) 
                        `(bind ,(car forms) 
                               #'(lambda(,_) 
                                   (declare (ignore ,_)) 
                                   ,(make-bind-seq (cdr forms))))))))) 
    (make-bind-seq forms))) 

;;; and define a generator facility 

(defconstant gen-nil (unit 'continuation nil)) 

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun generator-body (body) 
    (let ((escape (gensym))) 
      `(call/cc 
        #'(lambda (,escape) 
            (flet ((yield (x) 
                     (call/cc 
                      #'(lambda (escape1) 
                          (funcall ,escape (cons escape1 x)))))) 
              (monad-progn 
                ,@body 
                gen-nil))))))) 

(defmacro defgen (name args &body body) 
  `(defun ,name ,args 
     ,(generator-body body))) 

(defun next (generator &optional return-value) 
  (let ((gen-pair (run generator))) 
    (if (null gen-pair) 
        (values nil nil) 
        (destructuring-bind (gen . val) gen-pair 
          (values (funcall gen return-value) val))))) 

;;here's an example 
;;; the generator needs to written in a monadic style 
;;; defgen binds the function yield in the body 
(defgen leaves (tree) 
  (labels ((leaves (tree) 
             (cond ((null tree) 
                    gen-nil) 
                   ((and (null (cadr tree)) (null (cddr tree))) 
                    (yield (car tree))) 
                   (t (monad-progn 
                        (leaves (cadr tree)) 
                        (leaves (cddr tree))))))) 
    (leaves tree))) 

;;; the consumer uses next to iterate through the generator 
;;; the interface shares a lot of similarities with lazy-lists 
(defun same-fringe (t1 t2 &optional (pred #'eql)) 
  (labels ((luup (gen1 gen2) 
             (multiple-value-bind (gen1 val1) (next gen1) 
               (multiple-value-bind (gen2 val2) (next gen2) 
                 (cond ((and (null gen1) (null gen2)) t) 
                       ((or (null gen1) (null gen2)) nil) 
                       ((funcall pred val1 val2) 
                        (luup gen1 gen2)) 
                       (t nil)))))) 
    (or (eq t1 t2) 
        (luup (leaves t1) (leaves t2))))) 
