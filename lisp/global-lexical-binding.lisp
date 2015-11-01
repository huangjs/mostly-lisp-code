
(defun get-dynamic-cell (symbol)
  (or (get symbol 'dynamic-cell) (setf (get symbol 'dynamic-cell) (copy-symbol symbol))))

(defun dynamic-value (symbol) (symbol-value symbol))

(defmacro defv (var val)
  "Defines VAR to be a global dynamic variable with initial value VAL"
  `(progn
     (setf (symbol-value ',(get-dynamic-cell var)) ,val)
     (define-symbol-macro ,var (dynamic-value ',(get-dynamic-cell var)))))

(defmacro dval (var)
  "Returns the current dynamic binding of VAR, even if there is a lexical binding in scope"
  `(symbol-value ',(get-dynamic-cell var)))

(defmacro dlet (bindings &body body)
  "Unconditionally create new dynamic bindings"
  (if (atom bindings) (setf bindings `((,bindings ,(pop body)))))
  (let* ((vars (mapcar 'first bindings))
         (dvars (mapcar 'get-dynamic-cell vars))
         (vals (mapcar 'second bindings)))
    (dolist (v vars)
      (let ((e (macroexpand v)))
        (if (or (atom e) (not (eq (car e) 'dynamic-value)))
            (error "~A is not a dynamic variable" v))
        (if (eq (car e) 'non-settable-value) (error "~A is immutable" v))))
    `(let ,(mapcar 'list dvars vals) (declare (special ,@dvars)) ,@body)))

(defun get-lexical-cell (sym)
  (or (get sym 'lexical-cell) (setf (get sym 'lexical-cell) (copy-symbol sym))))

(defun non-settable-value (s) (symbol-value s))
(defun (setf non-settable-value) (val var)
  (declare (ignore val))
  (error "~A is immutable" var))

(defmacro defc (var val &optional force-rebind)
  "Immutably binds VAR to VAL.  If FORCE-REBIND is T then VAR is forcibly rebound."
  (let ((cell (get-lexical-cell var)))
    `(progn
       ,(if force-rebind
            `(setf (symbol-value ',cell) ,val)
            `(unless (boundp ',cell) (setf (symbol-value ',cell) ,val)))
       (define-symbol-macro ,var (non-settable-value ',cell)))))

(defmacro deflexical (var val)
  "Defines VAR to be a global lexical variable"
  (let ((cell (get-lexical-cell var)))
    `(progn
       (setf (symbol-value ',cell) ,val)
       (define-symbol-macro ,var (symbol-value ',cell)))))

(defmacro lval (var)
  "Unconditionally returns the global lexical binding of VAR"
  `(symbol-value ',(get-lexical-cell var)))


#| Examples:



? (defc constant1 "Constant value")
CONSTANT1
? (setf constant1 "Can't change a constant")
> Error: CONSTANT1 is immutable
> While executing: (SETF NON-SETTABLE-VALUE), in process Listener(6).
> Type cmd-. to abort, cmd-\ for a list of available restarts.
> Type :? for other options.
1 > 
? (defc constant1 "Can't change a constant value, take 2")
CONSTANT1
? constant1
"Constant value"
? (defc constant1 "Can rebind a constant by specifying FORCE-REBIND" t)
CONSTANT1
? constant1
"Can rebind a constant by specifying FORCE-REBIND"

? (defv v1 "Global dynamic variable")
V1
? (deflexical l1 "Global lexical variable")
L1
? (defun test1 () (list v1 l1))
TEST1
? (test1)
("Global dynamic variable" "Global lexical variable")
? (let ((v1 1) (l1 1)) (list v1 l1 (test1)))
(1 1 ("Global dynamic variable" "Global lexical variable"))
? (dlet ((v1 "Dynamic binding 1") (l1 "Dynamic binding 2")) (list v1 l1 (test1)))
("Dynamic binding 1" "Dynamic binding 2" ("Dynamic binding 1" "Global lexical variable"))
? (let ((v1 1)) (list v1 (dval v1)))
(1 "Global dynamic variable")
? (let ((l1 1)) (list l1 (lval l1)))
(1 "Global lexical variable")

                                        ; Watch this trick!
? (deflexical v1 "New global lexical binding for what was a dynamic variable")
V1
? (defun foo () v1)
FOO
? (let ((v1 1)) (list v1 (dval v1) (lval v1) (foo)))
(1 "Global dynamic variable" "New global lexical binding for what was a dynamic variable" "New global lexical binding for what was a dynamic variable")
? (dlet ((v1 1)) v1)
> Error: V1 is not a dynamic variable

|#
