(in-package "SB-C")

(defknown test-fun (t t &rest t) t)

(defun test-fun (x y)
  x)

(defun node-expression-tree (node)
  (cond ((ref-p node)
         (ref-leaf node))
        ((and (combination-p node)
              (eq (combination-kind node) :known))
         `(,(combination-fun-source-name node)
            ,@(mapcar (lambda (x)
                        (cond ((constant-lvar-p x)
                               (lvar-value x))
                              ((lvar-has-single-use-p x)
                               (node-expression-tree (lvar-use x)))
                              (t
                               x)))
                      (combination-args node))))
        (t
         node)))

(deftransform test-fun ((x y &rest z) * * :node node)
  (break "node: ~A~%~A~%" (node-expression-tree node) z)
  (abort-ir1-transform))

(defvar *bound-vars*)
(defvar *combination-vars*)

(defun emit-node-match-code (node-var pattern success fail)
  (cond ((and (symbolp pattern)
              (string= pattern "_"))
         success)
        ((and (symbolp pattern)
              (not (constantp pattern)))
         (assert (not (memq pattern *bound-vars*)))
         (push pattern *bound-vars*)
         `(progn
            (setf ,pattern ,node-var)
            ,success))
        ((atom pattern)
         `(if (and (constant-lvar-p ,node-var)
                   (eq ',pattern (lvar-value ,node-var)))
              ,success
              ,fail))
        (t
         (destructuring-bind (head . args) pattern
           (case head
             (quote
                `(if (and (constant-lvar-p ,node-var)
                          (equal ',(first args) (lvar-value ,node-var)))
                     ,success
                     ,fail))
             (or
                (cond
                  ((not (typep success '(cons symbol null)))
                   (let ((_success (gensym "SUCCESS")))
                     `(flet ((,_success ()
                               ,success))
                        ,(emit-node-match-code node-var pattern `(,_success) fail))))
                  ((null args)
                   fail)
                  (t
                   (emit-node-match-code
                    node-var
                    (first args)
                    success
                    (emit-node-match-code node-var
                                          `(or ,@(rest args))
                                          success
                                          fail)))))
             (and
                (cond
                  ((not (typep fail '(cons symbol null)))
                   `(let ((_fail (gensym "FAIL")))
                      `(flet ((,_fail ()
                                ,fail))
                         ,(emit-node-match-code node-var pattern success `(,_fail)))))
                  ((null args)
                   success)
                  (t
                   (emit-node-match-code
                    node-var
                    (first args)
                    (emit-node-match-code node-var
                                          `(and ,@(rest args))
                                          success
                                          fail)
                    fail))))
             (not
                (emit-node-match-code node-var
                                      (first args)
                                      fail
                                      success))
             (function
                (let ((name (first args)))
                  `(if (and (lvar-has-single-use-p ,node-var)
                            (let ((node (lvar-use ,node-var)))
                              (and (ref-p node)
                                   (let ((leaf (ref-leaf node)))
                                     (and (global-var-p leaf)
                                          (eq (global-var-kind leaf) :global-function)
                                          (equal ',name (leaf-source-name leaf)))))))
                       ,success
                       ,fail)))
             (:when
                 (destructuring-bind (condition &optional (pattern '_)) args
                   (let ((_fail (gensym "FAIL")))
                     `(flet ((,_fail ()
                               ,fail))
                        (if (,condition ,node-var)
                            ,(emit-node-match-code node-var pattern success `(,_fail))
                            (,_fail))))))
             (:type
                (destructuring-bind (type &optional (pattern '_)) args
                  (let ((_fail (gensym "FAIL")))
                    `(flet ((,_fail ()
                              ,fail))
                       (if (csubtypep (lvar-type ,node-var) (specifier-type ',type))
                           ,(emit-node-match-code node-var pattern success `(,_fail))
                           (,_fail))))))
             (:head-call
                (destructuring-bind (fun . args) args
                  (let ((_fail (gensym "FAIL"))
                        (_args (mapcar (lambda (x)
                                         (declare (ignore x))
                                         (gensym "ARG"))
                                       args)))
                    `(flet ((,_fail ()
                              ,fail))
                       (if (and (combination-p ,node-var)
                                (eq (combination-kind ,node-var) :known)
                                (eq ',fun (combination-fun-source-name ,node-var))
                                (= (length (combination-args ,node-var))
                                   ,(length args)))
                           (destructuring-bind ,_args (combination-args ,node-var)
                             ,(labels ((self (vars patterns success)
                                         (if (null vars)
                                             success
                                             (emit-node-match-code
                                              (first vars) (first patterns)
                                              (self (rest vars) (rest patterns) success)
                                              `(,_fail)))))
                                (self _args args success)))
                           (,_fail))))))
             (otherwise
                (let ((_node (gensym "NODE"))
                      (_fail (gensym "FAIL"))
                      (_combination (gensym "COMBINATION")))
                  (push _combination *combination-vars*)
                  `(flet ((,_fail ()
                            ,fail))
                    (if (lvar-has-single-use-p ,node-var)
                        (let ((,_node (lvar-use ,node-var)))
                          ,(emit-node-match-code _node `(:head-call ,head ,@args)
                                                 `(progn
                                                    (setf ,_combination ,node-var)
                                                    ,success)
                                                 `(,_fail)))
                        (,_fail))))))))
        (t (error "Malformed pattern ~S" pattern))))

(defun unconditionally-splice-fun-args (lvar)
  (let ((combination (lvar-use lvar)))
    (splice-fun-args lvar
                     (combination-fun-source-name combination)
                     (length (combination-args combination)))))

(defun lvar-use-eql (x y)
  (or (and (constant-lvar-p x)
           (constant-lvar-p y)
           (eq (lvar-value x) (lvar-value y)))
      (let ((x (lvar-uses x))
            (y (lvar-uses y)))
        (and (ref-p x)
             (ref-p y)
             (eql (ref-leaf x)
                  (ref-leaf y))))))

(defmacro %match-node ((node &key (fail '(give-up-ir1-transform)) (splice t)) clause
                       &body body)
  (declare (ignorable splice))
  (let* ((*bound-vars* '())
         (*combination-vars* '())
         (_node (gensym "NODE"))
         (_success (gensym "SUCCESS"))
         (form (emit-node-match-code _node
                                     `(:head-call ,@clause)
                                     `(,_success)
                                     fail)))
    `(let ((,_node ,node)
           ,@*bound-vars*
           ,@*combination-vars*)
       (flet ((,_success ()
                (multiple-value-call
                    (lambda (body &rest declarations)
                      ,@(mapcar (lambda (var)
                                  `(unconditionally-splice-fun-args ,var))
                                *combination-vars*)
                      (let* ((ignored-lvars
                              (loop for var in (combination-args ,_node)
                                    unless (or
                                             ,@(mapcar (lambda (var)
                                                         `(eq var ,var))
                                                       *bound-vars*))
                                      collect var))
                             (ignored-vars (mapcar (lambda (x)
                                                     (declare (ignore x))
                                                     (make-symbol "IGNORED"))
                                                   ignored-lvars)))
                        (setf (combination-args ,_node)
                              (list* ,@*bound-vars*
                                     ignored-lvars))
                        `(lambda (,@',*bound-vars* ,@ignored-vars)
                           (declare (ignore ,@ignored-vars))
                           ,@declarations
                           ,body)))
                  (let ,(mapcar (lambda (var)
                                  `(,var ,var))
                         *bound-vars*)
                    (declare (ignorable ,@*bound-vars*))
                    ,@body))))
         ,form))))

(defmacro match-node ((node &key (fail '(give-up-ir1-transform)))
                      &body clauses)
  (setf clauses (append clauses `((:fail ,fail))))
  (let ((_node (gensym "NODE"))
        (clause-funs (mapcar (lambda (_)
                               (declare (ignore _))
                               (gensym "CLAUSE"))
                             clauses)))
    `(let ((,_node ,node))
       (labels ,(loop for (pattern . body) in clauses
                      for (fun . funs) on clause-funs by #'cdr
                      collect
                      `(,fun ()
                             ,(if (null funs)
                                  `(progn ,@body)
                                  `(%match-node (,_node
                                                 :fail (,(first funs)))
                                       ,pattern ,@body))))
         (declare (ignorable ,@(mapcar (lambda (fun)
                                         `#',fun)
                                       clause-funs)))
         (,(first clause-funs))))))

(deftransform test-fun ((x y) * * :node node)
  (break "node: ~A~%" (node-expression-tree node))
  (match-node (node)
    ((test-fun #'+ x)
     (format t "Rewrite!~%")
     'x)))

(defknown %replace (t t index index) t)

(deftransform replace ((dst src &key start1 end1) * * :node node)
  "replace/subseq -> replace"
  (format t "node: ~A~%" (node-expression-tree node))
  (match-node (node)
    ((replace dst (subseq src start2 end2) :start1 start1 :end1 end1)
     `(replace dst src :start1 start1 :end1 end1 :start2 start2 :end2 end2))))

(defknown my-every (t sequence) boolean)

(deftransform my-every ((fn seq) (t simple-unboxed-array) t :node node)
  "every zerop -> optimise"
  (match-node (node)
    ((my-every (or 'zerop #'zerop) seq)
     (format t "rewrite!")
     (give-up-ir1-transform))))

(deftransform map-into ((dst fn x y) ((simple-array double-float 1)
                                      (or function symbol)
                                      (simple-array double-float 1)
                                      (simple-array double-float 1))
                        * :node node)
  "Parallelise map-into/+"
  (break "node: ~A~%" (node-expression-tree node))
  (match-node (node)
    ((map-into dst (or '+ #'+) (:when (lambda (x)
                                        (lvar-use-eql x dst)))
               y)
     (format t "rewrite!~%")
     (give-up-ir1-transform))))

(deftransform + ((x y) (rational rational) * :node node)
  "Constant-fold additions modulo associativity"
  (match-node (node)
    ((+ (+ x (:when constant-lvar-p y)) (:when constant-lvar-p z))
     (values `(+ x ,(+ (lvar-value y) (lvar-value z)))
             '(declare (ignore y z))))
    ((+ (+ (:type fixnum x) (:type (and rational (not fixnum)) y))
        (:type fixnum z))
     '(+ (+ x z) y))
    ((+ (:type fixnum z)
        (+ (:type fixnum x) (:type (and rational (not fixnum)) y)))
     '(+ (+ x z) y))))
