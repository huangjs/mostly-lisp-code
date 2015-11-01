(cl:in-package "SERIALISABLE-CLOSURES")

; We basically rebuild the whole environment,
; macros, funs, symbol-macros and variables
; (ugly hack for the macros), build the expression
; macroexpand it (modulo some things that have to be
; done from the compiler because they call our stuff),
; and remove all traces of macrolet/symbol-macrolet.
; We could use the environment for that, but it might
; make for some difference w/ illegal code between
; capture and noncaptured code, so I prefer not to.

(defun set-equal (x y)
  (let ((tablex (make-hash-table :test 'equal))
        (tabley (make-hash-table :test 'equal)))
    (dolist (x x)
      (setf (gethash x tablex) t))
    (dolist (y y)
      (setf (gethash y tabley) t)
      (unless (gethash y tablex)
        (return-from set-equal nil)))
    (dolist (x x)
      (unless (gethash x tabley)
        (return-from set-equal nil)))
    t))

(defun set-leq (xs ys)
  (let ((tabley (make-hash-table :test 'equal)))
    (dolist (y ys)
      (setf (gethash y tabley) t))
    (every (lambda (x)
             (gethash x tabley))
           xs)))

(defun funs-in-fun (fun)
  (sb-c::lexenv-funs (sb-c::functional-lexenv (cdr fun))))

(defun expand-macros (form env to-expand no-expand)
  "Selectively expands the macros in form."
  (let ((*walk-form-expand-macros-p* t))
    (walk-form form
               env
               (lambda (subform context env)
                 (declare (ignore context))
                 (cond ((atom subform) subform)
                       ((member (car subform)
                                no-expand)
                        (values subform t)) ; 2nd val = t -> don't recurse
                       (t (let ((entry (assoc (car subform) to-expand)))
                            (if entry
                                (funcall (cdr entry) subform env)
                                subform))))))))

(defun cluster-funs (funs)
  "Reverses the order of input list of functions
and clusters together labels groups."
  (let ((clusters '()))
    (labels ((find-cluster (fun funs)
               (let ((funs (remove fun funs)))
                 (if (and (consp (cdr fun))
                          (eq (second fun)
                              'sb-sys:macro))
                     (values (list 'macrolet fun)
                             funs)
                     (let* ((funs-in-fun (funs-in-fun fun))
                            (cluster (list fun))
                            (funs
                             (remove-if (lambda (fun2)
                                          (when (and (atom (cdr fun2))
                                                     (set-equal (funs-in-fun fun2)
                                                                funs-in-fun))
                                            (push fun2 cluster)
                                            t))
                                        funs)))
                       (if (member fun (funs-in-fun fun) :test 'equal)
                           (values (cons 'labels cluster) funs)
                           (values (cons 'flet cluster) funs))))))
             (inner (funs)
               (if (null funs)
                   clusters
                   (multiple-value-bind (cluster funs)
                       (find-cluster (first funs) funs)
                     (push cluster clusters)
                     (inner funs)))))
      (inner funs))))

(defun sort-clusters (clusters) ;; not used for now
  "Needed? Input should already be sorted"
  (sort (copy-list clusters)
        (lambda (c1 c2)
          (let ((funs1 (funs-in-fun (first c1)))
                (funs2 (funs-in-fun (first c2))))
            (and (not (set-equal funs1 funs2))
                 (set-leq funs1 funs2))))))

(defun vars-in-fun (fun)
  (sb-c::lexenv-vars
   (sb-c::functional-lexenv (cdr fun))))

(defun special-varp (var)
  (etypecase var
    (sb-c::lambda-var
     (sb-c::lambda-var-specvar var))
    (sb-c::global-var
     (eq :special (sb-c::global-var-kind var)))))

(defun make-form (clusters vars form &key ignore-unused-p &aux (closed-vars nil))
  "Given the clusters of function-esque definitions
the list of vars/symbol-macros returns a form that
recreates the lexical environment (using the temporary
vars associated with vars for their values) and splices
a form in it."
  (labels ((var-in-funp (var fun)
             (destructuring-bind (obj name tmp)
                 var
               (declare (ignore name tmp))
               (member obj (vars-in-fun fun) :key 'cdr :test 'eq)))
           (split-vars (vars acc)
             (cond ((null vars) (list (nreverse acc)))
                   ((consp (first (first vars)))
                    (list* (nreverse acc)
                           (first vars)
                           (split-vars (rest vars)
                                       nil)))
                   ((member (first vars) acc
                            :test (lambda (x y)
                                    (eq (second x)
                                        (second y))))
                    (cons (nreverse acc)
                          (split-vars vars nil)))
                   (t (split-vars (rest vars)
                                  (cons (first vars)
                                        acc)))))
           (emit-single-let (vars expr)
             (cond ((null vars)
                    expr)
                   ((typep vars
                           '(cons (cons (eql sb-sys:macro) t)
                                  (cons t (cons t null))))
                    (destructuring-bind ((marker . value) name tmp)
                        vars
                      (declare (ignore marker tmp))
                      `(symbol-macrolet ((,name ,value))
                       ,expr)))
                   (t
                    (let ((actual-vars (mapcan (lambda (var)
                                                 (destructuring-bind (obj name tmp)
                                                     var
                                                   (declare (ignore obj))
                                                   (when tmp
                                                     (push var closed-vars)
                                                     (list `(,name ,tmp)))))
                                               vars)))
                      `(let ,actual-vars
                           (declare
                            (ignorable ,@(mapcar 'first actual-vars))
                            ,@ (let ((specials
                                      (mapcan (lambda (var)
                                                (destructuring-bind (obj name tmp)
                                                    var
                                                  (declare (ignore tmp))
                                                  (when (special-varp obj)
                                                    (list name))))
                                              vars)))
                                 (when specials
                                   (list `(special ,@specials)))))
                         ,expr)))))
           (emit-vars (vars expr)
             (let ((groups (split-vars vars nil)))
               (reduce #'emit-single-let
                       groups
                       :from-end t
                       :initial-value expr)))
           (clean-tail-form (block-name forms)
             (let ((tail-form (first (last forms))))
               (if (typep tail-form
                          `(cons (eql block)
                                 (cons (eql ,block-name)
                                       list)))
                   (append (butlast forms)
                           (clean-tail-form block-name
                                            (cddr tail-form)))
                   forms)))
           (emit-funs (type funs expr)
             (ecase type
               ((macrolet)
                `(macrolet ,(mapcar (lambda (macro)
                                      (destructuring-bind (name _ . closure)
                                          macro
                                        (declare (ignore _))
                                        `(,name (&whole form &environment env)
                                                (funcall ,closure form env))))
                                    funs)
                   ,expr))
               ((flet labels)
                `(,type ,(mapcar (lambda (fun)
                                   (destructuring-bind (name . obj)
                                       fun
                                     (destructuring-bind (lambda args . body)
                                         (the (cons (eql lambda)
                                                    (cons list
                                                          list))
                                           (sb-c::functional-inline-expansion obj))
                                       (declare (ignore lambda))
                                       `(,name ,args ,@(clean-tail-form name
                                                                        body)))))
                                 funs)
                        (declare (ignorable ,@(mapcar (lambda (fun)
                                                        `#',(car fun))
                                                      funs)))
                        ,expr))))
           (inner ()
             (if (null clusters)
                 (if ignore-unused-p
                     form
                     (emit-vars vars form))
                 (let ((cluster (pop clusters))
                       (vars-to-emit '()))
                   (setf vars
                         (remove-if (lambda (var)
                                      (when (var-in-funp var
                                                         (second cluster))
                                        (push var vars-to-emit)
                                        t))
                                    vars))
                   (emit-vars (nreverse vars-to-emit)
                              (prog1
                                  (emit-funs (first cluster)
                                             (rest cluster)
                                             (inner))))))))
    (values (inner)
            (nreverse closed-vars))))

(defun remove-macros (expr)
  "We simply call the macro-function directly in
macrolets. Have to remove the definitions after
expanding them away, or else dumping will be hard,
and sharing of definitions harder."
  (labels ((inner (expr)
             (if (atom expr)
                 expr
                 (case (first expr)
                   ((macrolet symbol-macrolet)
                    (destructuring-bind (tag bindings expr . exprs)
                        expr
                      (declare (ignore tag bindings))
                      (if exprs
                          `(progn ,@(mapcar #'inner (cons expr exprs)))
                          (inner expr))))
                   ((flet let labels)
                    (destructuring-bind (tag bindings expr . exprs)
                        expr
                      `(,tag ,bindings
                             ,(inner expr)
                             ,@(mapcar #'inner exprs))))
                   (otherwise expr)))))
    (inner expr)))

(define-walker-template slambda sb-walker::walk-lambda) ; make sure sb-walker doesn't expand
(define-walker-template sfunction (nil quote))          ; slambda/sfunction (it doesn't store
                                                        ; enough information about local functions)
(defun make-eval-in-env (form env &key ignore-unused-p)
  "Returns a function of the values of
the lexical vars in env (ordered according to
the lexenv) -> value of form in that 
environment"
  (let* ((vars (sb-c::lexenv-vars env))
         (tmps (mapcar (lambda (var)
                         (destructuring-bind (name . obj)
                             var
                           (list obj name
                                 (unless (or (consp obj) ; symbol macro
                                             (special-varp obj))
                                   (gensym (symbol-name name))))))
                       vars)))
    (multiple-value-bind (form closed-vars)
        (make-form (cluster-funs (sb-c::lexenv-funs env))
                   tmps
                   form
                   :ignore-unused-p ignore-unused-p)
      (let* ((closed-vars  (nreverse closed-vars))
             (args (mapcar 'third closed-vars)))
        (values `(lambda ,args
                   (declare (ignorable ,@args))
                   ,(remove-macros      ; remove local macros
                     (sb-cltl2:macroexpand-all 
                      form)))
                (mapcar (lambda (var)
                          (destructuring-bind (name . obj)
                              var
                            (declare (ignore name))
                            (and (not (consp obj))
                                 (not (special-varp obj))
                                 (position obj closed-vars :key 'first :test 'eq))))
                        vars))))))

(defparameter *tag-builders* (make-hash-table)
  "tag -> (builder-src . builder-closure|nil)")
(defparameter *tag-counter* 0)

(defun expr-equal-modulo (expr1 expr2)
  "Are expr1 and expr2 equal modulo
substituted uninterned symbols?"
  (let ((x1->x2 (make-hash-table))
        (x2-subst (make-hash-table)))
    (labels ((inner (x1 x2)
               (cond ((and (consp x1)
                           (consp x2))
                      (and (inner (car x1)
                                  (car x2))
                           (inner (cdr x1)
                                  (cdr x2))))
                     ((or (consp x1)
                          (consp x2))
                      nil)
                     ((equal x1 x2)
                      t)
                     ((and (symbolp x1)
                           (symbolp x2)
                           (null (symbol-package x1))
                           (null (symbol-package x2)))
                      (or (eq (gethash x1 x1->x2)
                              x2)
                          (unless (or (gethash x2 x2-subst) ; already substituted. only do 1-1
                                      (gethash x1 x1->x2))
                            (setf (gethash x1 x1->x2) x2
                                  (gethash x2 x2-subst) t)))))))
      (inner expr1 expr2))))

(defun register-builder (build-expr)
  "Takes a builder expr, ensures it's in the
hash table, returns the corresponding tag,
and the builder expr"
  (block nil
    (maphash (lambda (tag builder)
               (destructuring-bind (src . closure)
                   builder
                 (declare (ignore closure))
                 (when (expr-equal-modulo build-expr
                                          src)
                   (return (values tag src)))))
             *tag-builders*)
    (let ((tag *tag-counter*))
      (incf *tag-counter*)
      (setf (gethash tag *tag-builders*)
            (cons build-expr nil))
      (values tag build-expr))))

(defun ensure-builder (builder-entry)
  (or (cdr builder-entry)
      (setf (cdr builder-entry)
            (compile nil (car builder-entry)))))

(defun ensure-all-builders ()
  (loop for i upfrom 0
     for entry = (gethash i *tag-builders*)
     while (or entry
               (< i *tag-counter*))
     do (unless (cdr entry)
          (setf (cdr entry)
                (compile nil (car entry))))))

(defclass serialisable-closure ()
  ((tag         :reader tag-of         :initarg :tag)
   (closed-vars :reader closed-vars-of :initarg :closed-vars)
   (build-expr  :reader build-expr-of  :initarg :build-expr))
  (:metaclass funcallable-standard-class))

(defmethod print-object ((obj serialisable-closure) stream)
  (let ((*package* (find-package "KEYWORD")))
    (format stream "#.~S" `(s:f ',(tag-of obj)
                                ',(closed-vars-of obj)))))

(defun thaw-sc (name args)
  (let ((builder (gethash name *tag-builders*)))
    (if builder
        (make-serialisable-closure
         (apply (ensure-builder builder)
                args)
         args
         nil
         name)
        (error "Unknown closure tag ~A" name))))

(defun s:f (name args)
  (thaw-sc name args))

(defun make-serialisable-closure (closure closed-vars expr &optional tag)
  (let ((sc (make-instance 'serialisable-closure
                           :closed-vars closed-vars
                           :build-expr  expr
                           :tag         tag)))
    (set-funcallable-instance-function sc closure)
    sc))

(defmacro %%make-env-list (ignore-unused-p positions
                           &environment env)
  (let* ((vars (sb-c::lexenv-vars env))
         (tmps (mapcar 'rest
                       (sort (loop
                                for position     in (reverse positions)
                                for (name . var) in (reverse vars)
                                when position
                                collect (list* position
                                               (gensym (symbol-name name))
                                               var))
                             '< :key 'first))))
    (setf (sb-c::lexenv-vars env)
          (append tmps
                  vars))
    `(list ,@(mapcar (if ignore-unused-p
                         (lambda (tmp)
                           (if (sb-c::lambda-var-refs (cdr tmp))
                               (car tmp)
                               ''s:_))
                         'car)
                     tmps))))

(defmacro make-env-list ((&key ignore-unused) &rest positions)
  (let ((x (gensym "RANDOM-BINDING")))
    `(let (,x) ;; needed to make sure we get a fresh lexenv object
       (declare (ignore ,x)) ;; just for the expression
       (%%make-env-list ,ignore-unused ,positions))))

(defmacro slambda ((&rest args) &body body)
  (let ((fn-name (gensym "LAMBDA-FN")))
    `(flet ((,fn-name (,@args) ,@body))
       (sfunction ,fn-name))))

(defmacro sfunction (function &environment env)
  (if (and (consp function)
           (eq (first function) 'lambda))
      `(slambda ,@(rest function))
      (multiple-value-bind (build-expr var-capturep)
          (make-eval-in-env `(function ,function)
                            env
                            :ignore-unused-p t)
        (multiple-value-bind (tag build-expr)
            (register-builder build-expr)
          `(make-serialisable-closure
            (function ,function)
            (make-env-list (:ignore-unused t) ,@var-capturep)
            ',build-expr
            ',tag)))))

;; BONUS!

(defun specialise (serialisable-closure
                   &optional (declare '(optimize speed)))
  (labels
      ((self (serialisable-closure)
         (let ((expr (build-expr-of serialisable-closure))
               (vals (mapcar (lambda (x)
                               (if (typep x 'serialisable-closure)
                                   (first (last (self x)))
                                   `(quote ,x)))
                             (closed-vars-of serialisable-closure))))
           (destructuring-bind (lambda args . body)
               expr
             (declare (ignore lambda))
             (let ((env (mapcar 'cons args vals)))
               (labels ((remove-from-tree (tree)
                          (cond ((consp tree)
                                 (list (mapcan #'remove-from-tree tree)))
                                ((assoc tree env)
                                 '())
                                (t
                                 (list tree))))
                        (replace-tree (tree)
                          (if (consp tree)
                              (if (eq (first tree)
                                      'declare)
                                  (car (remove-from-tree tree))
                                  (cons (replace-tree (car tree))
                                        (replace-tree (cdr tree))))
                              (let ((entry (assoc tree env :test 'eq)))
                                (if entry
                                    (cdr entry)
                                    tree)))))
                 `(lambda ()
                    (declare ,declare)
                    ,@(mapcar #'replace-tree body))))))))
    (let ((sb-ext:*evaluator-mode* :compile))
      ; weirdly, the funcall *in the compilation unit*
      ; is needed, or else stuff won't get inlined right.
      ; jsnell blames SLIME
      (eval `(funcall ,(self serialisable-closure))))))
