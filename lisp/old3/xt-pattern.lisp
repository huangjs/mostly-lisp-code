(cl:defpackage #:xt-pattern
  (:use #:cl)
  (:export #:match #:defclass-pattern #:fail))

(cl:in-package #:xt-pattern)

(defparameter *linearisation-table*
  (make-hash-table))

(defmacro deflineariser (name (&rest args) &body body)
  (let ((name (intern (symbol-name name) :keyword)))
    `(progn
       (setf (gethash ',name *linearisation-table*)
             (lambda ,args ,@body))
       ',name)))

(defparameter *position* '())

(defparameter *position-symbol*
  (make-hash-table :test 'equal))

(defclass var ()
  ((name :reader name-of :initarg :name :initform (gensym "VAR"))))

(defclass posn (var)
  ((posn :reader posn-of :initarg :posn)))

(defclass user-var (var)
  ((pattern-id :reader pattern-id-of :initarg :pattern-id)
   (given-name :reader given-name-of :initarg :given-name)))

(defmethod print-object ((obj posn) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~{ ~A~}" (posn-of obj))))

(defmethod print-object ((obj user-var) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A"
            (pattern-id-of obj)
            (given-name-of obj))))

(defun make-position (&optional (position *position*))
  (or (gethash position *position-symbol*)
      (setf (gethash position *position-symbol*)
            (make-instance 'posn :name (gensym "POSITION-NAME")
                                 :posn position))))

(define-symbol-macro posn (make-position))

(defparameter *pattern-uid* nil)
(defparameter *pattern-vars* nil)

(defun make-var (given-name &optional (uid *pattern-uid*))
  (assert (symbolp given-name))
  (or (gethash given-name *pattern-vars*)
      (setf (gethash given-name *pattern-vars*)
            (make-instance 'user-var :name (gensym "USER-VAR")
                                     :pattern-id uid
                                     :given-name given-name))))

(defmacro with-subpos (subpos &body body)
  `(let ((*position* (cons ',subpos *position*)))
     (declare (special *position*))
     ,@body))

(defmacro defmemo (name (&rest args) (&rest memo) &body body)
  (let ((_table (gensym "TABLE"))
        (_val   (gensym "VALUE"))
        (_foundp (gensym "FOUNDP"))
        (_memo  (gensym "MEMO")))
    `(progn
       (defparameter ,_table (make-hash-table :test 'equal))
       (defun ,name ,args
         (let ((,_memo (list ,@memo)))
           (multiple-value-bind (,_val ,_foundp)
               (gethash ,_memo ,_table)
             (if ,_foundp
                 ,_val
                 (setf (gethash ,_memo ,_table)
                       (block ,name
                         ,@body)))))))))

(defclass subclause ()
  ((posn :reader posn-of :initarg :posn :initform (make-position))))

(defmethod print-object ((obj subclause) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "posn: ~A" (posn-of obj))))

(defclass null-subclause (subclause)
  ())

(defmemo null-subclause () (posn)
  (make-instance 'null-subclause :posn posn))

(defclass bind-subclause (subclause)
  ((bound :reader bound-of :initarg :bound )))

(defclass var-bind-subclause (bind-subclause)
  ())

(defmemo var-bind-subclause (bound) (bound posn)
  (make-instance 'var-bind-subclause :bound bound))

(defmethod print-object ((obj bind-subclause) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "posn: ~A bound: ~A" (posn-of obj) (bound-of obj))))

(defclass var-equal-subclause (subclause)
  ((var :reader var-of :initarg :var)))

(defmemo var-equal-subclause (var) (var posn)
  (make-instance 'var-equal-subclause :var var :posn posn))

(defmethod print-object ((obj var-equal-subclause) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A = ~A" (posn-of obj) (var-of obj))))

(defclass wildcard-subclause (subclause)
  ())

(defmemo wildcard-subclause () (posn)
  (make-instance 'wildcard-subclause :posn posn))

(defclass equal-subclause (subclause)
  ((lit :reader lit-of :initarg :lit)))

(defmemo equal-subclause (lit) (lit posn)
  (make-instance 'equal-subclause :lit lit))

(defmethod print-object ((obj equal-subclause) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "posn: ~A lit: ~A" (posn-of obj) (lit-of obj))))

(defclass consp-subclause (subclause)
  ())

(defmemo consp-subclause () (posn)
  (make-instance 'consp-subclause))

(defclass bind-car (bind-subclause)
  ())

(defmemo bind-car (bound) (bound posn)
  (make-instance 'bind-car :bound bound))

(defclass bind-cdr (bind-subclause)
  ())

(defmemo bind-cdr (bound) (bound posn)
  (make-instance 'bind-cdr :bound bound))

(defclass action (subclause)
  ((bound-vars :reader bound-vars-of :initarg :bound-vars)
   (body       :reader body-of       :initarg :body)))

(defun linearise-clause (pattern action)
  (let* ((*pattern-uid* (gensym "PATTERN-UID"))
         (*pattern-vars* (make-hash-table :test 'eq))
         (subclauses (linearise pattern))
         (vars '()))
    (maphash (lambda (k var)
               (declare (ignore k))
               (push var vars))
             *pattern-vars*)
    (append subclauses
            (list (make-instance 'action
                   :bound-vars vars
                   :body       action)))))

(defun linearise (pattern)
  (if (null *pattern-vars*)
      (let ((*pattern-uid* (gensym "PATTERN-UID"))
            (*pattern-vars* (make-hash-table :test 'eq)))
        (linearise pattern)))
  (flet ((dispatch (pattern)
           (destructuring-bind (name . args)
               pattern
             (let ((fn (gethash name *linearisation-table*)))
               (if fn
                   (apply fn args)
                   (error "Unknown pattern type ~A" name))))))
    (dispatch (cond ((null pattern)    '(:null))
                    ((symbolp pattern) (if (string= "_" pattern)
                                           `(:wildcard)
                                           `(:var ,pattern)))
                    ((atom pattern)    `(:lit ,pattern))
                    ((eq (car pattern) 'quote)
                     (let ((lit (second pattern)))
                       (if (consp lit)
                           `(:cons ',(car lit) ',(cdr lit))
                           `(:lit ,lit))))
                    ((keywordp (car pattern))
                                       `,pattern)
                    (t                 `(:cons ,(car pattern)
                                               ,(cdr pattern)))))))

(defparameter *bound-vars* nil)

(defgeneric emit-subclause (subclause yes no))

(defun vars-in (expr)
  (cond ((typep expr 'var)
         (list expr))
        ((consp expr)
         (union (vars-in (car expr))
                (vars-in (cdr expr))))
        (t nil)))

(defgeneric subclause< (subclause-a subclause-b))

(defmethod subclause< (subclause-a subclause-b)
  nil)

(defmethod subclause< :around ((subclause-a subclause)
                               (subclause-b subclause))
  (and (not (eq subclause-a
                subclause-b))
       (eq (posn-of subclause-a)
           (posn-of subclause-b))
       (call-next-method)))

(defgeneric entailsp (a b mode))

(defgeneric contradictsp (a b mode))

(defmethod entailsp (a b mode)
  (if mode
      nil
      (contradictsp b a nil)))

(defmethod contradictsp (a b mode)
  (if mode
      nil
      (entailsp b a t)))

(defmethod entailsp :around (a b mode)
  (if (equal a b)
      mode
      (call-next-method)))

(defmethod contradictsp :around (a b mode)
  (if (equal a b)
      (not mode)
      (call-next-method)))

(defmethod contradictsp ((a null-subclause) (b consp-subclause)
                          (mode (eql t)))
  (eq (posn-of a)
      (posn-of b)))

(defmethod contradictsp ((a equal-subclause) (b consp-subclause)
                         (mode (eql t)))
  (and (eq (posn-of a)
           (posn-of b))
       (atom (lit-of a))))

(defmethod contradictsp ((a consp-subclause) (b null-subclause)
                         (mode (eql t)))
  (eq (posn-of a)
      (posn-of b)))

(defmethod contradictsp ((a consp-subclause) (b equal-subclause)
                         (mode (eql t)))
  (and (eq (posn-of a)
           (posn-of b))
       (atom (lit-of b))))

(defmethod contradictsp ((a equal-subclause) (b equal-subclause)
                         (mode (eql t)))
  (and (eq (posn-of a)
           (posn-of b))
       (not (equal (lit-of a)
                   (lit-of b)))))

(deflineariser null ()
  (list (null-subclause)))

(defmethod emit-subclause ((subclause null-subclause) yes no)
  `(if (null ,(posn-of subclause))
       ,(funcall yes)
       ,(funcall no)))

(deflineariser var (name)
  (multiple-value-bind (obj foundp)
      (gethash name *pattern-vars*)
    (if foundp
        (list (var-equal-subclause obj))
        (list (var-bind-subclause (make-var name))))))

(defmethod subclause< ((subclause-a var-bind-subclause)
                       (subclause-b var-bind-subclause))
  nil)

(defmethod subclause< (subclause-a (subclause-b var-bind-subclause))
  t)

(defmethod emit-subclause ((subclause var-bind-subclause) yes no)
  `(let ((,(bound-of subclause) ,(posn-of subclause)))
     ,(funcall yes)))

(defmethod emit-subclause ((subclause var-equal-subclause) yes no)
  `(if (equal ,(posn-of subclause)
              ,(var-of  subclause))
       ,(funcall yes)
       ,(funcall no)))

(deflineariser wildcard ()
  (list (wildcard-subclause))) ;; we need this for subclause<

(defmethod subclause< ((subclause-a wildcard-subclause)
                       (subclause-b wildcard-subclause))
  nil)

(defmethod subclause< (subclause-a (subclause-b wildcard-subclause))
  t)

(defmethod subclause< ((subclause-a var-bind-subclause) (subclause-b wildcard-subclause))
  nil)

(defmethod subclause< ((subclause-a wildcard-subclause)
                       (subclause-b var-bind-subclause))
  nil)

(defmethod emit-subclause ((subclause wildcard-subclause) yes no)
  (funcall yes))

(deflineariser lit (value)
  (list (if (null value)
            (null-subclause)
            (equal-subclause value))))

(defmethod emit-subclause ((subclause equal-subclause) yes no)
  `(if , (let ((posn (posn-of subclause))
               (lit  (lit-of  subclause)))
            (typecase lit
              ((or pathname cons bit-vector string) ; cons shouldn't happen
               `(equal ,posn ',lit))     ; equal descends on cons, string, bitvector, pathname
              ((or number                ; eql on chars, numbers
                   character)
               `(eql ,posn ',lit))
              (otherwise
               `(eq ,posn ',lit))))     ; eq otherwise
         ,(funcall yes)
         ,(funcall no)))

(deflineariser cons (car cdr)
  `(,(consp-subclause)
    ,(bind-car (make-position (cons 'car *position*)))
    ,@ (with-subpos car
         (linearise car))
    ,(bind-cdr (make-position (cons 'cdr *position*)))
    ,@ (with-subpos cdr
         (linearise cdr))))

(defmethod emit-subclause ((subclause consp-subclause) yes no)
  `(if (consp ,(posn-of subclause))
       ,(funcall yes)
       ,(funcall no)))

(defmethod emit-subclause ((subclause bind-car) yes no)
  (let ((next (funcall yes)))
    (if (member (bound-of subclause)
                (vars-in next))
        `(let ((,(bound-of subclause) (car ,(posn-of subclause))))
           ,next)
        next)))

(defmethod emit-subclause ((subclause bind-cdr) yes no)
  (let ((next (funcall yes)))
    (if (member (bound-of subclause)
                (vars-in next))
        `(let ((,(bound-of subclause) (cdr ,(posn-of subclause))))
           ,next)
        next)))

(defun split-declares-body (exprs)
  (let ((declares nil)
        (docstringp nil))
    (block nil
      (maplist (lambda (exprs)
                 (let ((exp (first exprs)))
                   (cond ((and (consp exp)
                               (eq (first exp) 'declare))
                          (push exp declares))
                         ((and (stringp exp)
                               (not docstringp))
                          (push exp declares)
                          (setf docstringp t))
                         (t (return (values (nreverse declares) exprs))))))
               exprs)
      (nreverse declares))))

(defmethod emit-subclause ((subclause action) yes no)
  (multiple-value-bind (declares body)
      (split-declares-body (body-of subclause))
    `(flet ((fail ()
              ,(funcall no)))
       (declare (ignorable #'fail))
       (let , (mapcar (lambda (var)
                        `(,(given-name-of var) ,var))
                      (bound-vars-of subclause))
         ,@declares
         (block nil
           ,@body)))))

#+(or) (defun pattern< (as bs)
         (and as
              bs
              (let ((first-a (first as))
                    (first-b (first bs)))
                (cond
                  ((some (lambda (b) (subclause< first-a b))
                         bs)
                   t)
                  ((some (lambda-list-keywords (a) (subclause< first-b a))
                         as)
                   nil)
                  (t (pattern< (rest as) (rest bs)))))))

(defun pattern< (a b)
  (flet ((stable-intersection (a b)
           (remove-if-not (lambda (x)
                            (member x b))
                          a)))
    (declare (ignorable #'stable-intersection))
    (let ((subclauses-a (make-hash-table))
          (posns-a      '())
          (subclauses-b (make-hash-table))
          (posns-b      '()))
      (dolist (subclause b
               (setf posns-b
                     (nreverse posns-b)))
        (let ((posn (posn-of subclause)))
          (pushnew posn posns-b)
          (push subclause (gethash posn subclauses-b))))
      (dolist (subclause a
               (setf posns-a
                     (nreverse posns-a)))
        (let ((posn (posn-of subclause)))
          (pushnew posn posns-a)
          (push subclause (gethash posn subclauses-a))))
      ;; That, or ~ lexico compare.
      ;; Current more conservative, but worse for
      ;; optimisation...
      #+(or) (dolist (posn (stable-intersection posns-a posns-b)
                      nil)
               (let ((as (gethash posn subclauses-a))
                     (bs (gethash posn subclauses-b)))
                 (when (every (lambda (b)
                                (some (lambda (a)
                                        (subclause< a b))
                                      as))
                              bs)
                   (return-from pattern< t))
                 (when (some (lambda (b)
                               (every (lambda (a)
                                        (subclause< b a))
                                      as))
                             bs)
                   (return-from pattern< nil))))
      #+(and) (let ((some< nil))
                (dolist (posn (intersection posns-a posns-b)
                         some<)
                  (let ((as (gethash posn subclauses-a))
                        (bs (gethash posn subclauses-b)))
                    (when (every (lambda (b)
                                   (some (lambda (a)
                                           (subclause< a b))
                                         as))
                                 bs)
                      (setf some< t))
                    (when (some (lambda (b)
                                  (every (lambda (a)
                                           (subclause< b a))
                                         as))
                                bs)
                      (return-from pattern< nil))))))))

(defun stable-set-difference (x y)
  (remove-if (lambda (x)
               (member x y))
             x))

(defun sort-patterns (linearised-patterns)
  "FIXME: fix me"
  (if (<= (length linearised-patterns)
          1)
      linearised-patterns
      (let ((adj (make-hash-table))
            (descendants '()))
        (maplist (lambda (patterns)
                   (destructuring-bind (p1 . patterns)
                       patterns
                     (map nil
                          (lambda (p2)
                            (when (pattern< p1 p2)
                              (push p2 descendants)
                              (push p2 (gethash p1 adj)))
                            (when (pattern< p2 p1)
                              (push p1 descendants)
                              (push p1 (gethash p2 adj))))
                          patterns)))
                 linearised-patterns)
        (labels
            ((get-descendants (root &optional marked)
               (if (member root marked)
                   '()
                   (cons
                    root
                    (remove root
                            (reduce #'union
                                    (mapcar (lambda (root)
                                              (get-descendants root marked))
                                            (gethash root adj))
                             :initial-value '()
                             :from-end t)))))
             (remove-marked (clusters marked)
               (remove nil
                       (mapcar (lambda (cluster)
                                 (stable-set-difference cluster marked))
                               clusters)))
             (get-pruned-descendants (roots marked)
               (mapcar (lambda (root)
                         (get-descendants root marked))
                       roots)))
          (let ((output '()))
            (labels
                ((sort-clusters (clusters marked)
                   (let ((clusters (remove-marked clusters
                                                  (union output marked))))
                     (when (null clusters)
                       (return-from sort-clusters nil))
                     (destructuring-bind ((pattern . patterns) . clusters)
                         (stable-sort clusters
                                      (lambda (a b)
                                        (cond ((/= (first a)
                                                   (first b))
                                               (> (first a)
                                                  (first b)))
                                              ((/= (second a)
                                                   (second b))
                                               (> (second a)
                                                  (second b)))
                                              (t (< (third a)
                                                    (third b)))))
                                      :key (lambda (cluster)
                                             (list (length
                                                    (set-difference
                                                     cluster
                                                     (apply #'append
                                                            output
                                                            (remove cluster
                                                                    clusters))))
                                                   (length cluster)
                                                   (position (first cluster)
                                                             linearised-patterns))))
                       (push pattern output)
                       (cond ((and (null clusters)
                                   (null patterns))
                              nil)
                             ((null patterns)
                              (sort-clusters clusters
                                             (cons pattern marked)))
                             (t (let ((to-emit (remove-if (lambda (pattern)
                                                            (some (lambda (cluster)
                                                                    (member pattern
                                                                            cluster))
                                                                  clusters))
                                                          patterns)))
                                  (sort-clusters
                                   (get-pruned-descendants
                                    to-emit
                                    (union output marked))
                                   (cons pattern
                                         (apply #'append
                                                marked
                                                clusters)))
                                  (sort-clusters clusters
                                                 (union output
                                                        marked)))))))))
              (sort-clusters (mapcar
                              #'get-descendants
                              (stable-set-difference linearised-patterns
                                                     descendants))
                             nil)
              (nreverse output)))))))

(defun drop-while (list pred)
  (labels ((self (list)
             (if (or (null list)
                     (not (funcall pred
                                   (first list))))
                 list
                 (self (rest list)))))
    (self list)))

(defun %emit-patterns (sorted-patterns fail bound-vars)
  (if (null sorted-patterns)
      (values fail (intersection bound-vars (vars-in fail)))
      (destructuring-bind (pattern . patterns)
          sorted-patterns
        (if (null pattern)
            (error "No more subclause in first pattern of ~A" sorted-patterns)
            (destructuring-bind (subclause . pattern)
                pattern
              (let* ((yes-bound-vars (if (typep subclause 'bind-subclause)
                                         (adjoin (bound-of subclause)
                                                 bound-vars)
                                         bound-vars))
                     (yes-patterns (cons pattern
                                         (mapcar (lambda (pattern)
                                                   (drop-while
                                                    pattern
                                                    (lambda (subclause2)
                                                      (entailsp subclause
                                                                subclause2
                                                                t))))
                                                 (drop-while
                                                  patterns
                                                  (lambda (pattern)
                                                    (some (lambda (subclause2)
                                                            (contradictsp subclause
                                                                          subclause2
                                                                          t))
                                                          pattern))))))
                     (yes-thunk (lambda ()
                                  (multiple-value-bind (yes-name yes-fv)
                                      (emit-patterns yes-patterns
                                                     fail
                                                     yes-bound-vars)
                                    (assert (subsetp yes-fv yes-bound-vars))
                                    `(,yes-name ,@yes-fv))))
                     (no-patterns (drop-while patterns
                                              (lambda (pattern)
                                                (some (lambda (subclause2)
                                                        (contradictsp subclause
                                                                      subclause2
                                                                      nil))
                                                      pattern))))
                     (no-thunk (lambda ()
                                 (multiple-value-bind (no-name no-fv)
                                     (emit-patterns no-patterns
                                                    fail
                                                    bound-vars)
                                   (assert (subsetp no-fv bound-vars))
                                   `(,no-name ,@no-fv)))))
                (let ((expr (let ((*bound-vars* bound-vars))
                              (emit-subclause subclause
                                              yes-thunk
                                              no-thunk))))
                  (values expr
                          (let ((fv (intersection bound-vars
                                                  (vars-in expr))))
                            (if (typep subclause 'bind-subclause)
                                (remove (bound-of subclause) fv)
                                fv))))))))))

(defparameter *state-cache*    nil)
(defparameter *permuted-cache* nil)

(defun emit-patterns (sorted-patterns
                      &optional (fail '(error "Match error"))
                                (bound-vars (list (make-position nil))))
  (if (null *state-cache*)
      (let ((*state-cache*    (make-hash-table :test 'equal))
            (*permuted-cache* (make-hash-table :test 'equal)))
        (multiple-value-bind (name fv)
            (emit-patterns sorted-patterns fail bound-vars)
          (let ((defns '()))
            (maphash (lambda (k v)
                       (declare (ignore k))
                       (destructuring-bind (name expr fv)
                           v
                         (push `(,name ,fv ,expr)
                               defns)))
                     *state-cache*)
            (assert (<= (length fv) 1))
            (values `(labels ,defns
                       (declare (ignorable
                                 ,@ (mapcar (lambda (defn)
                                              `(function ,(first defn)))
                                            defns)))
                       (,name ,@fv))
                    (or (first fv)
                        (gensym "INPUT"))))))
      (let ((key (cons sorted-patterns fail)))
        (multiple-value-bind (val foundp)
            (gethash key *state-cache*)
          (if foundp
              (destructuring-bind (name expr fv)
                  val
                (declare (ignore expr))
                (values name fv))
              (multiple-value-bind (expr fv)
                  (%emit-patterns sorted-patterns
                                  fail
                                  bound-vars)
                (multiple-value-bind (val foundp)
                    (gethash (cons expr fv) *permuted-cache*)
                  (if foundp
                      (values val fv)
                      (let ((name (gensym "STATE")))
                        (setf (gethash (cons expr fv) *permuted-cache*)
                              name
                              (gethash key *state-cache*)
                              (list name expr fv))
                        (values name fv))))))))))

(defun fixup-vars (expr)
  (cond ((typep expr 'var)
         (name-of expr))
        ((consp expr)
         (cons (fixup-vars (car expr))
               (fixup-vars (cdr expr))))
        (t
         expr)))

;;; putting it all together

(defmacro match ((to-match &optional (fail '(error "Match failure")))
                 &body clauses)
  (multiple-value-bind (expr top-var)
      (let* ((*position* '())
             (*position-symbol* (make-hash-table :test 'equal))
             (sorted-patterns (sort-patterns
                               (mapcar (lambda (clause)
                                         (destructuring-bind (pattern . action)
                                             clause
                                           (linearise-clause pattern action)))
                                       clauses))))
        (assert (= (length sorted-patterns)
                   (length clauses)))
        (emit-patterns sorted-patterns fail))
    (fixup-vars `(let ((,top-var ,to-match))
                   (declare (ignorable ,top-var))
                   ,expr))))

;;; Extending the pattern language

(defclass typep-subclause (subclause)
  ((type-test :reader type-test-of :initarg :type-test)))

(defmemo typep-subclause (type) (type posn)
  (make-instance 'typep-subclause :type-test type))

(deflineariser typep (type &optional (pattern '_))
  (cons (typep-subclause type)
        (linearise pattern)))

(defmethod subclause< ((a typep-subclause)
                       (b typep-subclause))
  (ignore-errors (subtypep (type-test-of a)
                           (type-test-of b))))

(defmethod subclause< ((a equal-subclause)
                       (b typep-subclause))
  (ignore-errors (typep (lit-of a)
                        (type-test-of b))))

(defmethod subclause< ((a consp-subclause)
                       (b typep-subclause))
  (ignore-errors (subtypep 'cons (type-test-of b))))

(defmethod entailsp (a
                     (b typep-subclause)
                     (mode (eql t)))
  (when (typep a '(or typep-subclause
                      equal-subclause
                      consp-subclause))
    (subclause< a b)))

(defmethod emit-subclause ((subclause typep-subclause) yes no)
  `(if (typep ,(posn-of subclause)
              ',(type-test-of subclause))
       ,(funcall yes)
       ,(funcall no)))

(defclass bind-slot (bind-subclause)
  ((slot :reader slot-of :initarg :slot)))

(defmemo bind-slot (slot bound) (slot bound posn)
  (make-instance 'bind-slot :bound bound :slot slot))

(deflineariser class (class &rest slot-test)
  (cons (typep-subclause class)
        (loop for (slot test) on slot-test by #'cddr
           append (cons (bind-slot slot (make-position (acons :slot slot *position*)))
                        (let ((*position* (acons :slot slot *position*)))
                          (declare (special *position*))
                          (linearise test))))))

(defmethod emit-subclause ((subclause bind-slot) yes no)
  (let ((next (funcall yes)))
    (if (member (bound-of subclause)
                (vars-in next))
        `(let ((,(bound-of subclause) (slot-value ,(posn-of subclause)
                                                  ',(slot-of subclause))))
           ,next)
        next)))

(defmacro defclass-pattern (class &optional (tag class) &rest positional-slots)
  (let ((temps (mapcar (lambda (slot)
                         (declare (ignore slot))
                         (gensym "SLOT"))
                       positional-slots)))
    `(deflineariser ,tag (&optional ,@(mapcar (lambda (temp)
                                                `(,temp '_))
                                              temps)
                                    &rest slots)
       (linearise (list* ':class ',class
                         ,@(mapcan (lambda (slot var)
                                     (list `',slot var))
                                   positional-slots temps)
                         slots)))))
#|
Testing

XT-PATTERN> (macroexpand '(match (23)
                           ((:typep integer) 'int)
                           (23 42)))
(LET ((#:POSITION-NAME1925 23))
  (LABELS ((#:STATE1931 (#:POSITION-NAME1925)
             (IF (EQUAL #:POSITION-NAME1925 23)
                 (#:STATE1929)
                 (#:STATE1930 #:POSITION-NAME1925)))
           (#:STATE1930 (#:POSITION-NAME1925)
             (IF (TYPEP #:POSITION-NAME1925 'INTEGER)
                 (#:STATE1928)
                 (#:STATE1927)))
           (#:STATE1929 ()
             (FLET ((FAIL ()
                      (#:STATE1928)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ()
                 (BLOCK NIL 42))))
           (#:STATE1928 ()
             (FLET ((FAIL ()
                      (#:STATE1927)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ()
                 (BLOCK NIL 'INT))))
           (#:STATE1927 ()
             (ERROR "Match failure")))
    (DECLARE
     (IGNORABLE #'#:STATE1931 #'#:STATE1930 #'#:STATE1929 #'#:STATE1928
      #'#:STATE1927))
    (#:STATE1931 #:POSITION-NAME1925)))

=> 42

XT-PATTERN> (macroexpand '(match (23)
                           ((:typep fixnum) 'fixnum)
                           ((:typep integer) 'integer)))
(LET ((#:POSITION-NAME2720 23))
  (LABELS ((#:STATE2726 (#:POSITION-NAME2720)
             (IF (TYPEP #:POSITION-NAME2720 'FIXNUM)
                 (#:STATE2724)
                 (#:STATE2725 #:POSITION-NAME2720)))
           (#:STATE2725 (#:POSITION-NAME2720)
             (IF (TYPEP #:POSITION-NAME2720 'INTEGER)
                 (#:STATE2723)
                 (#:STATE2722)))
           (#:STATE2724 ()
             (FLET ((FAIL ()
                      (#:STATE2723)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ()
                 (BLOCK NIL 'FIXNUM))))
           (#:STATE2723 ()
             (FLET ((FAIL ()
                      (#:STATE2722)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ()
                 (BLOCK NIL 'INTEGER))))
           (#:STATE2722 ()
             (ERROR "Match failure")))
    (DECLARE
     (IGNORABLE #'#:STATE2726 #'#:STATE2725 #'#:STATE2724 #'#:STATE2723
      #'#:STATE2722))
    (#:STATE2726 #:POSITION-NAME2720)))

=> FIXNUM

XT-PATTERN> (macroexpand '(match ('(23 23))
                           ((23 _ . x) x)
                           ((23 23) (format t "next!~%")
                                    (fail))))
(LET ((#:POSITION-NAME2623 '(23 23)))
  (LABELS ((#:STATE2644 (#:POSITION-NAME2623)
             (IF (CONSP #:POSITION-NAME2623)
                 (#:STATE2643 #:POSITION-NAME2623)
                 (#:STATE2630)))
           (#:STATE2643 (#:POSITION-NAME2623)
             (LET ((#:POSITION-NAME2624 (CAR #:POSITION-NAME2623)))
               (#:STATE2642 #:POSITION-NAME2623 #:POSITION-NAME2624)))
           (#:STATE2642 (#:POSITION-NAME2623 #:POSITION-NAME2624)
             (IF (EQUAL #:POSITION-NAME2624 23)
                 (#:STATE2641 #:POSITION-NAME2623)
                 (#:STATE2630)))
           (#:STATE2641 (#:POSITION-NAME2623)
             (LET ((#:POSITION-NAME2625 (CDR #:POSITION-NAME2623)))
               (#:STATE2640 #:POSITION-NAME2625)))
           (#:STATE2640 (#:POSITION-NAME2625)
             (IF (CONSP #:POSITION-NAME2625)
                 (#:STATE2639 #:POSITION-NAME2625)
                 (#:STATE2630)))
           (#:STATE2639 (#:POSITION-NAME2625)
             (LET ((#:POSITION-NAME2626 (CAR #:POSITION-NAME2625)))
               (#:STATE2638 #:POSITION-NAME2625 #:POSITION-NAME2626)))
           (#:STATE2638 (#:POSITION-NAME2625 #:POSITION-NAME2626)
             (IF (EQUAL #:POSITION-NAME2626 23)
                 (#:STATE2637 #:POSITION-NAME2625)
                 (#:STATE2634 #:POSITION-NAME2625)))
           (#:STATE2637 (#:POSITION-NAME2625)
             (LET ((#:POSITION-NAME2627 (CDR #:POSITION-NAME2625)))
               (#:STATE2636 #:POSITION-NAME2625 #:POSITION-NAME2627)))
           (#:STATE2636 (#:POSITION-NAME2625 #:POSITION-NAME2627)
             (IF (NULL #:POSITION-NAME2627)
                 (#:STATE2635 #:POSITION-NAME2625)
                 (#:STATE2634 #:POSITION-NAME2625)))
           (#:STATE2635 (#:POSITION-NAME2625)
             (FLET ((FAIL ()
                      (#:STATE2634 #:POSITION-NAME2625)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ()
                 (BLOCK NIL (FORMAT T "next!~%") (FAIL)))))
           (#:STATE2634 (#:POSITION-NAME2625)
             (#:STATE2633 #:POSITION-NAME2625))
           (#:STATE2633 (#:POSITION-NAME2625)
             (LET ((#:POSITION-NAME2627 (CDR #:POSITION-NAME2625)))
               (#:STATE2632 #:POSITION-NAME2627)))
           (#:STATE2632 (#:POSITION-NAME2627)
             (LET ((#:USER-VAR2628 #:POSITION-NAME2627))
               (#:STATE2631 #:USER-VAR2628)))
           (#:STATE2631 (#:USER-VAR2628)
             (FLET ((FAIL ()
                      (#:STATE2630)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ((X #:USER-VAR2628))
                 (BLOCK NIL X))))
           (#:STATE2630 ()
             (ERROR "Match failure")))
    (DECLARE
     (IGNORABLE #'#:STATE2644 #'#:STATE2643 #'#:STATE2642 #'#:STATE2641
      #'#:STATE2640 #'#:STATE2639 #'#:STATE2638 #'#:STATE2637 #'#:STATE2636
      #'#:STATE2635 #'#:STATE2634 #'#:STATE2633 #'#:STATE2632 #'#:STATE2631
      #'#:STATE2630))
    (#:STATE2644 #:POSITION-NAME2623)))

=>
next!
NIL

XT-PATTERN> (macroexpand '(match ('(23 . 23))
                           ((x . y) y)
                           ((x . x) 'same)))
(LET ((#:POSITION-NAME2646 '(23 . 23)))
  (LABELS ((#:STATE2663 (#:POSITION-NAME2646)
             (IF (CONSP #:POSITION-NAME2646)
                 (#:STATE2662 #:POSITION-NAME2646)
                 (#:STATE2653)))
           (#:STATE2662 (#:POSITION-NAME2646)
             (LET ((#:POSITION-NAME2647 (CAR #:POSITION-NAME2646)))
               (#:STATE2661 #:POSITION-NAME2646 #:POSITION-NAME2647)))
           (#:STATE2661 (#:POSITION-NAME2646 #:POSITION-NAME2647)
             (LET ((#:USER-VAR2652 #:POSITION-NAME2647))
               (#:STATE2660 #:POSITION-NAME2646 #:POSITION-NAME2647
                #:USER-VAR2652)))
           (#:STATE2660 (#:POSITION-NAME2646 #:POSITION-NAME2647 #:USER-VAR2652)
             (LET ((#:POSITION-NAME2649 (CDR #:POSITION-NAME2646)))
               (#:STATE2659 #:POSITION-NAME2646 #:POSITION-NAME2647
                #:USER-VAR2652 #:POSITION-NAME2649)))
           (#:STATE2659
               (#:POSITION-NAME2646 #:POSITION-NAME2647 #:USER-VAR2652
                #:POSITION-NAME2649)
             (IF (EQUAL #:POSITION-NAME2649 #:USER-VAR2652)
                 (#:STATE2658 #:POSITION-NAME2646 #:POSITION-NAME2647
                              #:USER-VAR2652)
                 (#:STATE2657 #:POSITION-NAME2646 #:POSITION-NAME2647)))
           (#:STATE2658 (#:POSITION-NAME2646 #:POSITION-NAME2647 #:USER-VAR2652)
             (FLET ((FAIL ()
                      (#:STATE2657 #:POSITION-NAME2646 #:POSITION-NAME2647)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ((X #:USER-VAR2652))
                 (BLOCK NIL 'SAME))))
           (#:STATE2657 (#:POSITION-NAME2646 #:POSITION-NAME2647)
             (LET ((#:USER-VAR2648 #:POSITION-NAME2647))
               (#:STATE2656 #:POSITION-NAME2646 #:USER-VAR2648)))
           (#:STATE2656 (#:POSITION-NAME2646 #:USER-VAR2648)
             (LET ((#:POSITION-NAME2649 (CDR #:POSITION-NAME2646)))
               (#:STATE2655 #:POSITION-NAME2649 #:USER-VAR2648)))
           (#:STATE2655 (#:POSITION-NAME2649 #:USER-VAR2648)
             (LET ((#:USER-VAR2650 #:POSITION-NAME2649))
               (#:STATE2654 #:USER-VAR2648 #:USER-VAR2650)))
           (#:STATE2654 (#:USER-VAR2648 #:USER-VAR2650)
             (FLET ((FAIL ()
                      (#:STATE2653)))
               (DECLARE (IGNORABLE #'FAIL))
               (LET ((Y #:USER-VAR2650) (X #:USER-VAR2648))
                 (BLOCK NIL Y))))
           (#:STATE2653 ()
             (ERROR "Match failure")))
    (DECLARE
     (IGNORABLE #'#:STATE2663 #'#:STATE2662 #'#:STATE2661 #'#:STATE2660
      #'#:STATE2659 #'#:STATE2658 #'#:STATE2657 #'#:STATE2656 #'#:STATE2655
      #'#:STATE2654 #'#:STATE2653))
    (#:STATE2663 #:POSITION-NAME2646)))

=> SAME
|#
