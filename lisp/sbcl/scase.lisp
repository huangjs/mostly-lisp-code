(in-package "SB-C")

;; Dies if it's actually emitted.
(define-vop (die)
    (:info      reason)
  (:generator 666
	      (apply 'compiler-error reason)))

(defmacro die (&optional (string "die must not be called") &rest arguments)
  `(%primitive die '(,string ,@arguments)))

;; Fake function. First argument is a constant, the # of values to
;; dispatch on followed by the right # of values, and an arbitrary
;; number of cases, constant list of types and a function.
;; Cases that come first are assumed to be preferable.

(defknown %scase (unsigned-byte &rest t) t)

(deftransform %scase ((nargs &rest data) * *
                      :node node)
  (aver (constant-lvar-p nargs))
  (let* ((nargs (lvar-value nargs))
         (args  (subseq data 0 nargs))
         (arg-types (mapcar 'lvar-type args))
         (_args (mapcar (lambda (arg)
                          (declare (ignore arg))
                          (gensym "ARG"))
                        args))
         (_cases ())
         (cases (loop for (case) on (subseq data nargs)
                   by #'cddr
                   collect (let ((case-var (gensym "CASE"))
                                 (fn-var   (gensym "FN")))
                             (push case-var _cases)
                             (push fn-var   _cases)
                             (aver (constant-lvar-p case))
                             `(,(mapcar 'specifier-type
                                        (lvar-value case))
                                ,fn-var
                                ,(lvar-value case)))
                   finally (setf _cases
                                 (nreverse _cases))))
         (delta-p nil)
         (maximal nil)
         (new-cases (remove-if-not
		     (lambda (case)
		       (let ((types (first case)))
			 (when (and (not maximal)
				    (every 'csubtypep
					   arg-types types))
			   (setf maximal case)
			   (or (every 'types-equal-or-intersect
				      arg-types types)
			       (prog1 nil
				 (setf delta-p t))))))
		     cases))
         (wait-for `(,@ (when (eq t (component-reoptimize
                                     (node-component node)))
                          `(:optimize))
                        ,@ (when (eq t (component-reanalyze
                                        (node-component node)))
                             `(:constraint)))))
    #+nil(format t "arg types: ~A optimize: ~A analyze: ~A deltap: ~A~%" arg-types
		 (component-reoptimize (node-component node))
		 (component-reanalyze (node-component node))
		 delta-p)
    (cond ((and (null new-cases)
                (not maximal))
           (compiler-error "node case applicable in %scase"))
          ((or (null (rest new-cases))
               (and (null wait-for)
                    (not delta-p))
               (eq maximal (first new-cases)))
           (unless maximal
             (compiler-error "no case applicable in %scase"))
           `(lambda (nargs ,@_args ,@_cases)
              (declare (ignore nargs)
                       (ignorable ,@_cases)
                       ,@(mapcar (lambda (arg type)
                                   `(type ,type ,arg))
                                 _args (third maximal)))
              (funcall ,(second maximal) ,@_args)))
          (delta-p
           `(lambda (nargs ,@_args ,@_cases)
              (declare (ignore nargs)
                       (ignorable ,@_cases))
              (%scase ,nargs ,@_args
                      ,@(mapcan
                         (lambda (case)
                           (destructuring-bind (types fn-var list)
                               case
                             (declare (ignore types))
                             `(',list ,fn-var)))
                         new-cases))))
          (t         (apply 'delay-ir1-transform node wait-for)))))


;;; syntax:
;;; (scase ([let-style bindings]) [case]*) case: (([type for 1st binding] [... 2nd binding]...) form*)
(in-package "CL-USER")
(defmacro scase ((&rest args)
                 &body cases)
  (let* (ignores
         arg-vals arg-vars)
    (dolist (arg args)
      (multiple-value-bind (val arg)
          (if (atom arg)
              (let ((ignore (gensym "IGNORE")))
                (push ignore ignores)
                (values arg ignore))
              (destructuring-bind (var val)
                  arg
                (values val var)))
        (push val arg-vals)
        (push arg arg-vars)))
    (setf arg-vals (nreverse arg-vals)
          arg-vars (nreverse arg-vars))
    `(sb-c::%scase ,(length args) ,@arg-vals
                   ,@(mapcan (lambda (case)
                               (destructuring-bind (types &body body)
                                   case
                                 (list `',types
                                       `(lambda ,arg-vars
                                          (declare (ignore ,@ignores)
						   ,@(mapcar (lambda (arg type)
							       `(type ,type ,arg))
							     arg-vars types))
                                          ,@body))))
			     cases)
		   ',(make-list (length args) :initial-element t)
		   (lambda ,arg-vars
		     (declare (ignore ,@arg-vars))
		     (sb-c::die "unable to dispatch scase")))))
