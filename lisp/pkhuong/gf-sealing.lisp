#|
 Generic function sealing for SBCL.

 Untested, use at your own risks. Patches and improvements
 obviously welcome. I would love to see this get to a
 state such that it could be merged in SBCL's mainline.

 Once a GF has been sealed, the system assumes that
 its method and whatever relevant part of the class
 hierarchy will not be modified, and uses that additional
 static information to optimise the dispatch of the
 generic function.

 Note that this will obviously not work with SBCL's user-defined
 specialisers (which currently require a custom g-f class anyway).
 However, I have plans to make it possible to use custom specialisers
 in standard methods, and to make that work with sealing.

 The dispatch will also be specialised and inlined at
 every call site by default. Specialisation is of course
 expected to improve performance, but inlining itself may
 also help branch prediction. Inlining also improve the
 precision of inferred of return types. If inlining is
 disabled, a conservative estimate is still used. It might
 be possible to compute as tight a return type as with
 inlining without doing as much work as computing a
 discrimination net. That remains to be seen.

 To disable inlining, use NOTINLINE, or declare SPACE >= SPEED
 or SPACE >= 2.

 Methods of class INLINED-SEALED-METHOD that have been defined at the
 toplevel environment will also have their body completely
 inlined in the dispatching code. This allows Python to further
 optimise the method's body with better type information.
 If a generic-function is declared to be of class INLINED-SEALED-GF,
 (with :generic-function-class), its method will be of class
 INLINED-SEALED-METHOD by default.

 Once a generic function's method and all related classes have
 been defined, the generic function can be sealed with the macro
 SEAL-GF. (seal-gf [generic-function-name]) will seal the generic
 function and replace it with a specialised function.
 (seal-gf [g-f-name] [sealed-function-name]) will seal the generic
 function in function [sealed-function-name].

 seal-gf also takes some keyword arguments to better control inlining
 of code. When :infer-only is true, no specialised function is
 created. The generic definition is kept around. Inlined discrimination
 nets will however still be generated as usual. :max-methods gives an
 upper-bound for the (estimated) number of methods for which a discrimin-
 ation net will be generated. :max-methods-inline gives an upper-bound
 for the maximal number of inlinable methods (inlined-sealed-method) that
 will actually be inlined. The latter two upper bounds are only respected
 in inlined call sites, and not for the global specialised definition
 (if any).

 Summary:
 1. Define generic functions of class inlined-sealed-gf for inlined methods
 2. Use notinline to do a full call to the dispatch,
     or declare SPACE >= SPEED, or SPACE >= 2.
    Use SPACE > 0 for inlined discrimination net, but not
     method bodies.
 3. (seal-gf [generic-function-name] &optional [sealed-function-name]
             &key infer-only max-methods max-methods-inline)
     must be called *before* compiling or evaluating any
     code that should take advantage of sealing.

 Paul Khuong (pvk@pvk.ca), 2007-07-07

2009-10-27 Minor bugfixes. Seems to compile fine on 1.0.32.

2007-07-08 Better control over inlining, with two more
     keyword parameters for seal-gf, max-methods and
     max-methods-inline. :MAX-METHODS gives the maximal
     number of estimated applicable methods (which is never
     underestimated) for which to inline a discrimination net.
     :MAX-METHODS-INLINE is the maximal number of inlinable
     applicable methods for which to enable inlining. If there
     are too many inlinable methods, inlining is disabled
     altogether. These could not be optimisation qualities,
     since they do fit in the range [0, 3].

     Type propagation is now a bit tighter when inlining
     is disabled.

2007-07-07 Building a discrimination net still seems lossy.
     In order to improve compilation speed and program size,
     seal-gf now takes a key argument, :INFER-ONLY to signal that
     the program should only try to infer types, and to inline
     on demand. When :INFER-ONLY is true, no new function with
     a specialised discrimination net is generated, so gf-name
     must equal new-name.
   Influence of policy:
      Type propagation is always active
      Marking the gf notinline will disable any inlining,
        both of the discrimination net and of the methods.
      A discrimination net will only be inlined and specialised
       if (> SPEED SPACE) and (< SPACE 2).
      Inlinable method bodies are not inlined when (> SPACE 0).

2007-07-07 Thanks to Kevin Reid for his testing & fixes:
    Fixed some stupid bugs
    Moved from alists to hash tables in make-discriminating-expr to improve speed
    Now using find-method and load-time-value to avoid dumping literal
     methods and functions in code.
    Changed the names of sealed-method and sealed-gf to inline-sealed-...
     to emphasize the fact that they will enable inlining of method bodies.

2007-07-07 Initial Release
          
|#

(cl:defpackage "SEALED-GF"
  (:use "CL" "SB-PCL")
  (:export #:inlined-sealed-method #:inlined-sealed-gf #:seal-gf))

(cl:in-package "SEALED-GF")

;;; I'm a bit greedy here. I assume standard methods, and who knows what else...

(defclass inlined-sealed-method (standard-method)
  ((lambda-expr :initarg :lambda-expr :reader inlined-sealed-method-lambda-expr)))

(defvar *gf-name* nil
  "Holds the name of the generic function. Used to indirect
look-ups in load-time-value (by method-lambda-expr) and avoid
literal non-loadable objects.")

(defun make-find-method (method)
  `(find-method #',*gf-name*
                ',(method-qualifiers method)
                ',(method-specializers method)))

(defgeneric method-lambda-expr (method type)
  (:documentation " Given a method object and type,
 returns a lambda expr that does, if type is:
:slow     a normal call of the method-function
:fast     a call of the method-fast-function without pv-cell
:pv-table a call of the method-fast-function with a pv-cell
:inline   the body of the method

Each type has a different calling convention."))

(defmethod method-lambda-expr ((method standard-method)
                               (type (eql :slow)))
  `(lambda (args next-method)
     (funcall (sb-ext:truly-the function
                                (load-time-value (method-function ,(make-find-method method))))
              args next-method)))

(defmethod method-lambda-expr ((method standard-method)
                               (type (eql :fast)))
  (assert (and (null (sb-pcl::method-plist-value method :pv-table))
               (sb-pcl::safe-method-fast-function method)))
  (let ((fmf `(sb-ext:truly-the function
                                (load-time-value (sb-pcl::safe-method-fast-function ,(make-find-method method))))))
    (multiple-value-bind (req-args restp)
        (required-args-from-lambda-list (sb-pcl:method-lambda-list method))
      (if restp
          `(lambda (next-methods ,@req-args &rest .rest.)
             (apply ,fmf nil next-methods ,@req-args
                    .rest.))
          `(lambda (next-methods ,@req-args)
             (funcall ,fmf nil next-methods ,@req-args))))))

(defmethod method-lambda-expr ((method standard-method)
                               (type (eql :pv-table)))
  (assert (and (sb-pcl::method-plist-value method :pv-table)
               (sb-pcl::safe-method-fast-function method)))
  (let ((fmf `(sb-ext:truly-the function
                                (load-time-value (sb-pcl::safe-method-fast-function ,(make-find-method method))))))
    (multiple-value-bind (req-args restp)
        (required-args-from-lambda-list (sb-pcl:method-lambda-list method))
      (if restp
          `(lambda (pv-cell next-methods ,@req-args &rest .rest.)
             (apply ,fmf pv-cell next-methods ,@req-args
                    .rest.))
          `(lambda (pv-cell next-methods ,@req-args)
             (funcall ,fmf pv-cell next-methods ,@req-args))))))

(defmethod method-lambda-expr ((method inlined-sealed-method)
                               (type (eql :inline)))
  (assert (slot-boundp method 'lambda-expr))
  (destructuring-bind (let ((var (lambda-type . lambda-args))
                            . bindings)
                        . expr)
      (third (sb-pcl::make-method-initargs-form-internal
              (inlined-sealed-method-lambda-expr method)
              nil nil))
    (declare (ignore let var bindings expr))
    `(lambda ,@(if (eq lambda-type 'sb-int:named-lambda)
                   (rest lambda-args)
                   lambda-args))))

(defun method-lambda-type (method)
  (cond ((and (typep method 'inlined-sealed-method) ; we can only inline if we have the info
              (slot-boundp method 'lambda-expr))
         :inline)
        ((not (sb-pcl::safe-method-fast-function method))
         :slow)
        ((sb-pcl::method-plist-value method :pv-table)
         :pv-table)
        (t
         :fast)))

(defun make-next-method-call (next-methods)
  (when (first next-methods)
    (let ((nm (first next-methods))
          (nms (rest next-methods)))
      (sb-pcl::make-method-call
       :function (if (sb-pcl::std-instance-p nm)
                     (method-function nm)
                     nm)
       :call-method-args (list nms)))))

(defclass inlined-sealed-gf (standard-generic-function) ()
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class (find-class 'inlined-sealed-method))
  (:documentation "Make generic functions of that class
for methods to be of class inlined-sealed-method automagically"))

(defmethod make-method-lambda ((proto-gf inlined-sealed-gf) proto-method
                               lambda-expr env)
  "Save the lambda expr iff we are in the null
environment. Could extend to local macros and
functions, but that'd be a PITA for little gain,
probably."
  (if (sb-c::null-lexenv-p env)
      (multiple-value-bind (lambda-expr initargs)
          (call-next-method)
        (values lambda-expr
                (list* :lambda-expr lambda-expr
                       initargs)))
      (call-next-method)))

;; end-greedy-part

(defun required-args-from-lambda-list (arg-list)
  (loop for arg in arg-list
        when (member arg lambda-list-keywords :test #'eq)
          return (values args t)
        collect arg into args
        finally (return (values args nil))))

(defun required-args-of (gf)
  (required-args-from-lambda-list (generic-function-lambda-list gf)))

(defun widen-types-for-pcl (type &optional simple-typesp)
  "Really for specializer-applicable-using-type-p. Should
just fix that function. Quick hack for now

simple-typesp: if t, have to use class, class-eq, prototype, eql. 
PCL's inference doesn't let us compose types arbitrarily"
  (flet ((inner (type simple-typesp)
           (if (atom type)
               `(class ,(find-class type))
               (destructuring-bind (ctor . args)
                   type
                 (case ctor
                   ((member) (if (= 1 (length args))
                                 `(eql ,(first args))
                                 (let* ((types (mapcar 'class-of args))
                                        (unif (find-if (lambda (type1)
                                                         (every (lambda (type2)
                                                                  (subtypep type2 type1))
                                                                types))
                                                       types)))
                                   (if unif
                                       `(class ,unif)
                                       t))))
                   ((and)    (unless simple-typesp
                               `(and ,@(mapcar (lambda (type)
                                                 (widen-types-for-pcl type simple-typesp))
                                               args))))
                   ((not)    (unless simple-typesp
                               (let ((type (widen-types-for-pcl (first args) t)))
                                 (if (eq type t)
                                     t
                                     `(not ,type)))))
                   ((eql)    type)
                   (t (if (subtypep type 'number)
                          (cond ((and (numberp (first args))
                                      (eql (first args) (second args)))
                                 `(eql ,(first args)))
                                ((subtypep type 'fixnum)
                                 `(class ,(find-class 'fixnum)))
                                ((subtypep type 'integer)
                                 `(class ,(find-class 'integer)))
                                ((subtypep type 'ratio)
                                 `(class ,(find-class 'ratio)))
                                ((subtypep type 'short-float)
                                 `(class ,(find-class 'short-float)))
                                ((subtypep type 'single-float)
                                 `(class ,(find-class 'single-float)))
                                ((subtypep type 'double-float)
                                 `(class ,(find-class 'double-float)))
                                ((subtypep type 'long-float)
                                 `(class ,(find-class 'long-float)))
                                ((subtypep type 'float)
                                 `(class ,(find-class 'float)))
                                ((subtypep type 'real)
                                 `(class ,(find-class 'real)))
                                ((subtypep type 'complex)
                                 `(class ,(find-class 'complex)))
                                (t
                                 `(class ,(find-class 'number))))
                          t)))))))
    (or (ignore-errors (inner type simple-typesp))
        t)))

(defun pcl-type-to-type (pcl-type)
  (if (atom pcl-type)
      t
      (destructuring-bind (ctor . args)
          pcl-type
        (case ctor
          ((and not) `(,ctor ,@(mapcar 'pcl-type-to-type args)))
          ((class class-eq) (first args))
          ((eql)  pcl-type)
          (t      t)))))

;; All this code would be much cleaner with
;; sb-kernel actual type (as opposed to type *specifiers*)
;; manipulation functions. Unfortunately, most
;; of the code currently assumes type specifiers,
;; and that is what %simple-fun-type also returns.
;; To Fix Later.

(defun refine-args-var-type (types pcl-types restp)
  "Returns a type for the list of arguments, given inferred
types from Python (types) and from the decision tree (pcl-types)"
  (reduce (lambda (type acc)
            `(cons ,type ,acc))
          (if types
              (mapcar (lambda (type pcl-type)
                        `(and ,type
                              ,(pcl-type-to-type pcl-type)))
                      types pcl-types)
              (mapcar 'pcl-type-to-type pcl-types))
          :initial-value (if restp
                             t
                             'null)
          :from-end t))

(defun method-return-type (method)
  (let ((f-f (sb-pcl::safe-method-fast-function method)))
    (when f-f
      (let ((type (sb-kernel:%simple-fun-type f-f)))
        (if (typep type '(cons (eql function) (cons t (cons t t))))
            (let ((type (third type)))
              (if (typep type '(cons (eql values) t))
                  type
                  `(values ,type)))
            `(values))))))

(defun method-arguments-types (method &optional simplep)
  "simplep -> we only want the required arguments' types"
  (let ((f-f (sb-pcl::safe-method-fast-function method)))
    (when f-f
      (let ((type (sb-kernel:%simple-fun-type f-f)))
        (if (listp type)
            (let ((types (cddr (second type)))) ; get argument types, skip pv-cell, next-method
              (if simplep
                  (let ((end-simple (position-if (lambda (x)
                                                   (member x lambda-list-keywords))
                                                 types)))
                    (values (subseq types 0 (or end-simple
                                                (length types)))
                            end-simple))
                  types))
            nil)))))

(defun make-discriminating-expr (gf args-var &optional types no-inlinep)
  "Hair... Build the actual discrimination tree,
with local functions for all the methods.
Takes a gf-object, a name for the var w/ the list of all args
and optionally the inferred type of the arguments.
Assumes any &rest list is in .rest.
Returns

An expr that does the discrimination and calls the correct method,
list of expected (required) argument types,
and whether args-var is actually needed."
  (multiple-value-bind (required-args restp)
      (required-args-of gf)
    (let* ((sorted-methods (sb-pcl::sort-applicable-methods
                            (sb-pcl::compute-precedence (sb-mop:generic-function-lambda-list gf)
                                                        (length required-args)
                                                        (sb-mop:generic-function-argument-precedence-order gf))
                            (if types
                                (sb-pcl::compute-applicable-methods-using-types
                                 gf
                                 (mapcar 'widen-types-for-pcl types))
                                (copy-list (sb-mop:generic-function-methods gf)))
                            (make-list (length required-args) :initial-element t)))
           (methods-info (mapcar (lambda (method)
                                   (list method
                                         (gensym "METHOD")
                                         (let ((type (method-lambda-type method)))
                                           (if (and no-inlinep
                                                    (eq type :inline))
                                               :fast
                                               type))))
                                 sorted-methods))
           (counter      0)
           (effective-methods-table (make-hash-table))
           (method-return-types-table (make-hash-table))
           (method-arguments-types-table (make-hash-table))
           (sb-pcl::*in-precompute-effective-methods-p* t)
           (compiled-net (sb-pcl::generate-discrimination-net-internal
                          gf sorted-methods (mapcar 'widen-types-for-pcl types)
                          (lambda (methods known-types)
                            (when methods
                              (let ((id (incf counter))
                                    (method (first methods)))
                                (setf (gethash method effective-methods-table)
                                      id
                                      (gethash (rest (method-return-type method))
                                               method-return-types-table)
                                      id
                                      (gethash (method-arguments-types method t)
                                               method-arguments-types-table)
                                      id)))
                            `(sb-pcl::methods ,methods ,known-types))
                          (lambda (position type true-value false-value)
                            (let ((arg (elt required-args position)))
                              (if (eq (car type) 'eql)
                                  (let* ((false-case-p (and (consp false-value)
                                                            (or (eq (car false-value)
                                                                    'sb-pcl::scase)
                                                                (eq (car false-value)
                                                                    'sb-pcl::mcase))
                                                            (eq arg (cadr false-value))))
                                         (false-clauses (if false-case-p
                                                            (cddr false-value)
                                                            `((t ,false-value))))
                                         (case-sym (if (and (sb-pcl::dnet-methods-p true-value)
                                                            (if false-case-p
                                                                (eq (car false-value)
                                                                    'sb-pcl::mcase)
                                                                (sb-pcl::dnet-methods-p
                                                                 false-value)))
                                                       'sb-pcl::mcase
                                                       'sb-pcl::scase))
                                         (type-sym `(,(cadr type))))
                                    `(,case-sym ,arg
                                                (,type-sym ,true-value)
                                                ,@false-clauses))
                                  `(if ,(let ((arg (elt required-args position)))
                                          (case (car type)
                                            (class    (let ((class (second type)))
                                                        (if (null (class-direct-subclasses class))
                                                            `(sb-pcl::class-eq-test ,arg ,class)
                                                            `(sb-pcl::class-test    ,arg ,class))))
                                            (class-eq `(sb-pcl::class-eq-test ,arg ,(cadr type)))))
                                       ,true-value
                                       ,false-value))))
                          #'identity))
           effective-methods method-return-types method-arguments-types)
      (flet ((listify-table (table)
               (let ((result (mapcar 'car (sort (let (list)
                                                  (maphash (lambda (k v)
                                                             (push (cons k v)
                                                                   list))
                                                           table)
                                                  list)
                                                '<
                                                :key #'cdr))))
                 result)))
        (setf effective-methods      (listify-table effective-methods-table)
              method-return-types    (listify-table method-return-types-table)
              method-arguments-types (listify-table method-arguments-types-table)))
      (unless effective-methods
        (if types
            (sb-c:compiler-style-warn
             "No effective method found for ~S~% when called with arguments of types ~S"
             gf types)
            (sb-c:compiler-style-warn
             "No effective method found for ~S~%"
             gf)))
      (values `(flet ,(mapcar (lambda (method-info)
                                `(,(second method-info)
                                  ,@(rest (method-lambda-expr
                                           (car method-info)
                                           (third method-info)))))
                       methods-info)
                 (declare (inline ,@(mapcar 'second methods-info))
                          (ignorable ,@(mapcar (lambda (info)
                                                 `(function ,(second info)))
                                               methods-info)))
                 (macrolet
                     ((sb-pcl::methods (methods pcl-types)
                        (declare (optimize (speed 0))
                                 (sb-ext:muffle-conditions sb-ext:compiler-note))
                        (let* ((method (first methods))
                               (methods (rest methods))
                               (args-expr  (list 'the
                                                 (refine-args-var-type ',types
                                                                       pcl-types
                                                                       ',restp)
                                                 ',args-var))
                               (types ,(if types
                                           `(mapcar (lambda (type pcl-type)
                                                      `(and ,type
                                                            ,(pcl-type-to-type pcl-type)))
                                                    ',types pcl-types)
                                           `(mapcar 'pcl-type-to-type pcl-types)))
                               (info (gethash method ', (let ((table (make-hash-table)))
                                                          (dolist (entry methods-info table)
                                                            (setf (gethash (car entry) table)
                                                                  (cdr entry))))))
                               (name (first info))
                               (type (second info))
                               (args ',required-args))
                          (if (null method)
                              (if (not ',restp)
                                  (list* 'no-applicable-method
                                         ',gf args)
                                  (list* 'apply ''no-applicable-method
                                         ',gf
                                         (append args '(.rest.))))
                              `(locally
                                   (declare (optimize speed))
                                 ,(if (not ',restp)
                                      (ecase type
                                        ((:slow)    `(,name ,args-expr ',methods))
                                        ((:fast)    `(,name ',(make-next-method-call methods)
                                                            ,@(mapcar (lambda (type arg)
                                                                        `(sb-ext:truly-the ,type ,arg))
                                                                      types args)))
                                        ((:pv-table)
                                         (let ((pv-table (sb-pcl::method-plist-value method :pv-table)))
                                           (assert pv-table)
                                           `(,name (sb-pcl::pv-table-lookup-pv-args ',pv-table ,@args)
                                                   ',(make-next-method-call methods)
                                                   ,@(mapcar (lambda (type arg)
                                                               `(sb-ext:truly-the ,type ,arg))
                                                             types args))))
                                        ((:inline)  `(,name nil
                                                            ',(make-next-method-call methods)
                                                            ,@(mapcar (lambda (type arg)
                                                                        `(sb-ext:truly-the ,type ,arg))
                                                                      types args))))
                                      (ecase type
                                        ((:slow)    `(,name ,args-expr ',methods))
                                        ((:fast)    `(apply #',name
                                                            ',(make-next-method-call methods)
                                                            ,@(mapcar (lambda (type arg)
                                                                        `(sb-ext:truly-the ,type ,arg))
                                                                      types args)
                                                            .rest.))
                                        ((:pv-table)
                                         (let ((pv-table (sb-pcl::method-plist-value method :pv-table)))
                                           (assert pv-table)
                                           `(apply #',name
                                                   (sb-pcl::pv-table-lookup-pv-args ',pv-table ,@args)
                                                   ',(make-next-method-call methods)
                                                   ,@(mapcar (lambda (type arg)
                                                               `(sb-ext:truly-the ,type ,arg))
                                                             types args)
                                                   .rest.)))
                                        ((:inline)  `(apply #',name
                                                            nil
                                                            ',(make-next-method-call methods)
                                                            ,@(mapcar (lambda (type arg)
                                                                        `(sb-ext:truly-the ,type ,arg))
                                                                      types args)
                                                            .rest.)))))))))
                   (sb-ext:truly-the
                    ,(if (every 'listp method-return-types)
                         `(values ,@ (let* ((exact-values-p t)
                                            (cleaned-types (mapcar (lambda (type)
                                                                     (let ((posn-optional (position '&optional type)))
                                                                       (when (or (not posn-optional)
                                                                                 (< posn-optional (1- (length type))))
                                                                         (setf exact-values-p nil))
                                                                       (subseq type 0
                                                                               (or posn-optional
                                                                                   (length type)))))
                                                                   method-return-types)))
                                       (append (and cleaned-types
                                                    (apply 'mapcar (lambda (&rest types)
                                                                     `(or ,@types))
                                                           cleaned-types))
                                               (when exact-values-p
                                                 '(&optional)))))
                         '*)
                    ,compiled-net)))
              (when method-arguments-types ; expected input types
                (if types
                    (apply 'mapcar (lambda (type &rest types)
                                     `(and ,type
                                           (or ,@types)))
                           types
                           method-arguments-types)
                    (apply 'mapcar (lambda (&rest types)
                                     `(or ,@types))
                           method-arguments-types)))
              (some (lambda (method) ; does the args list need to be built at all?
                      (or (eq (method-lambda-type method)
                              :slow)
                          (eq (method-lambda-type method)
                              :pv-table)))
                    effective-methods)))))

(defun make-lambda-expr (gf &optional types (args nil argsp) rest-arg no-inlinep)
  "Wraps make-discriminating-expr.
Takes a gf, list of inferred types, name of req args, and the name of the rest arg if any
returns a lambda expr with the right names that does dispatch.
Note that make-discriminating-expr assumes any rest var is named .rest.
-> brittle."
  (multiple-value-bind (required-args any-otherp)
      (required-args-of gf)
    (unless argsp
      (setf args required-args))
    (unless rest-arg
      (setf rest-arg (and any-otherp
                          '.rest.))))
  (let ((arg-list (gensym "ARGS"))
        (*gf-name* (generic-function-name gf)))
    (multiple-value-bind (body args-types args-varp)
        (make-discriminating-expr gf arg-list types no-inlinep)
      `(lambda (,@args ,@ (when rest-arg
                            `(&rest ,rest-arg)))
         (declare ,@(mapcar (lambda (arg type)
                              `(type ,type ,arg))
                            args args-types))
         (let ((,arg-list ,(if args-varp
                               `(list* ,@args ,rest-arg)
                               nil)))
           (declare (ignorable ,arg-list)
                    ,@ (when args-varp
                         `((optimize speed sb-c::stack-allocate-dynamic-extent)
                           (dynamic-extent ,arg-list))))
           ,body)))))

(defmacro define-deftransform (name gf-name &optional max-methods max-methods-inline)
  (multiple-value-bind (required-args any-otherp)
      (required-args-of (fdefinition gf-name))
    (let ((rest '.rest.))
      `(progn
         (sb-c:defknown ,name (,@(make-list (length required-args) :initial-element t)
                               ,@(when any-otherp
                                   '(&rest list)))
             *)
         
         (sb-c:deftransform ,name ((,@required-args ,@(when any-otherp
                                                        `(&rest ,rest)))
                                   * *
                                   :policy (and (> speed space)
                                                (< space 2))
                                   :node   node)
           (let* ((applicable-methods
                    (multiple-value-bind (applicable-methods definite-p)
                        (sb-pcl::compute-applicable-methods-using-types
                         #',gf-name
                         (list ,@ (mapcar (lambda (arg)
                                            `(widen-types-for-pcl
                                              (sb-kernel:type-specifier
                                               (sb-c::lvar-type ,arg))))
                                          required-args)))
                      (if definite-p
                          (list (first applicable-methods))
                          applicable-methods)))
                  (num-methods   (length applicable-methods))
                  (num-inlinable (count-if (lambda (method)
                                             (eq (method-lambda-type method)
                                                 :inline))
                                           applicable-methods)))
             (declare (ignorable num-methods num-inlinable))
             (flet ((body ()
                      (destructuring-bind (lambda args declare body)
                          (let ((*gf-name* ',gf-name))
                            (make-lambda-expr #',gf-name
                                              (list ,@(mapcar (lambda (arg)
                                                                `(sb-kernel:type-specifier
                                                                  (sb-c::lvar-type ,arg)))
                                                              required-args))
                                              ',required-args
                                              ', (when any-otherp
                                                   rest)
                                                 ,(if max-methods-inline
                                                      `(or (> num-inlinable ,max-methods-inline)
                                                           (sb-c:policy node
                                                                        (> space 0)))
                                                      `(sb-c:policy node
                                                                    (> space 0)))))
                        (declare (ignore lambda args))
                        (values body (list declare)))))
               ,(if max-methods
                    `(if (> num-methods ,max-methods)
                         (sb-c::give-up-ir1-transform)
                         (body))
                    '(body)))))
         
         (sb-c:defoptimizer (,name derive-type)
             ((,@required-args ,@(when any-otherp
                                   `(&rest ,rest))))
           (multiple-value-bind (applicable-methods definite-p)
               (sb-pcl::compute-applicable-methods-using-types
                #',gf-name
                (list ,@ (mapcar (lambda (arg)
                                   `(widen-types-for-pcl
                                     (sb-kernel:type-specifier
                                      (sb-c::lvar-type ,arg))))
                                 required-args)))
             (when definite-p
               (setf applicable-methods (list (first applicable-methods))))
             (let ((return-types (mapcar 'method-return-type
                                         applicable-methods)))
               (if (and applicable-methods
                        (every 'identity return-types))
                   (reduce (lambda (t1 t2)
                             (sb-kernel:values-type-union t1 t2))
                           (mapcar 'sb-kernel:values-specifier-type return-types))
                   (sb-kernel:specifier-type t)))))))))

(defmacro seal-gf (gf-name &optional (new-name gf-name) &key (infer-only nil) max-methods max-methods-inline)
  "INFER-ONLY is really badly named, since it also enables the inlining
of specialised dispatch code"
  (declare (type (or null integer) max-methods max-methods-inline))
  (if infer-only
      (progn
        (assert (equal gf-name new-name))
        `(progn
           (define-deftransform ,new-name ,gf-name ,max-methods ,max-methods-inline)
           ',new-name))
      (let ((gf (sb-pcl::gdefinition gf-name)))
        (when (eq new-name gf-name)
          (let ((name (gensym "GENERIC-FUNCTION")))
            (setf (fdefinition name) gf
                  gf-name            name)))
        (destructuring-bind (lambda args . body)
            (let ((*gf-name* gf-name))
              (make-lambda-expr gf))
          (declare (ignore lambda))
          #+nil(fmakunbound gf-name)
          `(progn
             (defun ,new-name ,args
               #+nil(declare (optimize speed))
               ,@body)
             (define-deftransform ,new-name ,gf-name ,max-methods ,max-methods-inline)
             ',new-name)))))
