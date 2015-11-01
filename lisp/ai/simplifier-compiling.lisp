;;; define a benchmark test suite

(defvar *test-data* (mapcar #'infix->prefix
  '((d (a * x ^ 2 + b * x + c) / d x)
    (d ((a * x ^ 2 + b * x + c) / x) / d x)
    (d ((a * x ^ 3 + b * x ^ 2 + c * x + d) / x ^ 5) / d x)
    ((sin (x + x)) * (sin (2 * x)) + (cos (d (x ^ 2) / d x)) ^ 1)
    (d (3 * x + (cos x) / x) / d x))))

(defvar *answers* (mapcar #'simplify *test-data*))

(defun test-it (&optional (with-profiling t))
  "Time a test run, and make sure the answers are correct."
  (let ((answers
         (if with-profiling
             (prof:with-profiling ()
               (mapcar #'simplify *test-data*))
             (mapcar #'simplify *test-data*))))
    (mapc #'assert-equal answers *answers*)
    t))

(defun assert-equal (x y)
  "If x is not equal to y, complain."
  (assert (equal x y) (x y)
          "Expected ~a to be equal to ~a" x y))

;;; indexing
(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic,
or by using the simp function supplied for this operator.
This version indexes simplification rules under the operator."
  (cond ((rule-based-translator exp (rules-for (exp-op exp))  ;***
                                :rule-if #'exp-lhs
                                :rule-then #'exp-rhs
                                :action #'(lambda (bindings response)
                                            (simplify (sublis bindings response)))))
        ((evaluable? exp) (eval exp))
        (t exp)))

(defvar *rules-for* (make-hash-table :test #'eq))

(defun main-op (rule) (exp-op (exp-lhs rule)))

(defun index-rules (rules)
  "Index all the rules under the main op."
  (clrhash *rules-for*)
  (dolist (rule rules)
    ;; nconc instead of push to preserve the order of rules
    (setf (gethash (main-op rule) *rules-for*)
          (nconc (gethash (main-op rule) *rules-for*)
                 (list rule)))))

(defun rules-for (op) (gethash op *rules-for*))

(index-rules *simplification-rules*)


;;; compilation
;;; e.g., rule (x + x = 2 * x)
;;; with the above rule indexed under "+", it should be compiled into
;;; (lambda (exp)
;;;   (if (equal (exp-lhs exp) (exp-rhs exp))
;;;       (make-exp :op '* :lhs 2 :rhs (exp-rhs exp))))

;;; a better approach
;;; compile a set of rules

;;; single rule compiler
(defvar *bindings* nil
  "A list of bindings used by the rule compiler.")

(defun compile-rule (rule)
  "Compile a single rule."
  (let ((*bindings* nil))
    `(lambda (x)
      ,(compile-exp 'x (exp-lhs rule)   ; x is the lambda parameter
                    (delay (build-exp (exp-rhs rule)
                                      *bindings*))))))

(defun compile-exp (var pattern consequent)
  ;; consequent is a continuation function,
  ;; it means the continuation for generating the code is the test passes.
  "Compile code that tests the expression, and does consequent
if it matches. Assumes bindings in *bindings*."
  (cond ((get-binding pattern *bindings*)
         ;; Test a previously bound variable
         `(if (equal ,var ,(lookup pattern *bindings*))
              ,(force consequent)))
        ((variable? pattern)
         ;; Add a new bindings; do type checking if needed.
         (push (cons pattern var) *bindings*)
         (force consequent))
        ((atom pattern)
         ;; Match a literal atom
         `(if (eql ,var ',pattern)
              ,(force consequent)))
        ((starts-with pattern '?is)
         (push (cons (second pattern) var) *bindings*)
         `(if (,(third pattern) ,var)
              ,(force consequent)))
        ;; So, far, only the ?is pattern is covered, because
        ;; it is the only one used in simplification rules.
        ;; Other patterns could be compiled by adding code here.
        ;; Or we could switch to a data-driven approach.
        (t ;; Check the oprerator and arguments
         `(if (op? ,var ',(exp-op pattern))
              ,(compile-args var pattern consequent)))))

(defun compile-args (var pattern consequent)
  "Compile code that checks the arg or args, and
does consequent if the arg(s) match."
  ;; First make up variable names for the arg(s).
  (let ((L (symbol var 'L))
        (R (symbol var 'R)))
    (if (exp-rhs pattern)
        ;; two arg case
        `(let ((,L (exp-lhs ,var))
               (,R (exp-rhs ,var)))
          ,(compile-exp L (exp-lhs pattern)
                        (delay
                         (compile-exp R (exp-rhs pattern)
                                      consequent))))
        ;; one arg case
        `(let ((,L (exp-lhs ,var)))
          ,(compile-exp L (exp-lhs pattern) consequent)))))

(defun build-exp (exp bindings)
  "Compile code that will build the exp, given the bindings."
  (cond ((assoc exp bindings) (rest (assoc exp bindings)))
        ((variable? exp)
         (error "Variable ~a occurred on right-hand side, but not left." exp))
        ((atom exp) `',exp)
        (t (let ((new-exp (mapcar #'(lambda (x)
                                      (build-exp x bindings))
                                  exp)))
             `(simplify-exp (list ,@new-exp))))))

(defun op? (exp op)
  "Does the exp have the given op as its operator?"
  (and (exp? exp) (eq (exp-op exp) op)))

(defun exp? (x)
  "Is x a valid expression?"
  (listp x))

(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol."
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol."
  (make-symbol (format nil "~{~a~}" args)))


;;; rule-set compiler
(defun compile-rule-set (op)
  "Compile all rules indexed under a given main op,
and make them into the simp-fn for that op."
  (set-simp-fn op
    (compile nil
      `(lambda (x)
        ,(reduce #'combine-rules
                 (mapcar #'compile-indexed-rule
                         (rules-for op)))))))

(defun compile-indexed-rule (rule)
  "Compile one rule into lambda-less code,
assuming indexing of main op."
  (let ((*bindings* nil))
    (compile-args
     'x (exp-lhs rule)
     (delay (build-exp (exp-rhs rule) *bindings*)))))

(defun combine-rules (a b)
  "Combine the code for two rules into one, maintaining order."
  ;; In the default case, we generate the code (or a b),
  ;; but we try to be cleverer and share common code,
  ;; on the assumption that there are no side-effects.
  ;; Note: preserve the order of the rules is important!!
  (cond ((and (listp a) (listp b)
              (= (length a) (length b) 3)
              (equal (first a) (first b))
              (equal (second a) (second b)))
         ;; a=(f x y), b = f(x z) => (f x (combine-rules y z))
         ;; this can apply when f=IF or f=LET
         (list (first a) (second a)
               (combine-rules (third a) (third b))))
        ((matching-ifs a b)
         `(if ,(second a)
              ,(combine-rules (third a) (third b))
              ,(combine-rules (fourth a) (fourth b))))
        ((starts-with a 'or)
         ;; a = (or ... (if p y)), b = (if p z)
         ;; => (or ... (if p (combine-rules y z)))
         ;; else
         ;; a =  (or ...) b=> (or ... b)
         (if (matching-ifs (last1 a) b)
             (append (butlast a)
                     (list (combine-rules (last1 a) b)))
             (append a (list b))))
        (t ;; a, b => (or a b)
         `(or ,a ,b))))

(defun matching-ifs (a b)
  "Are a and b if statements with the same predicate?"
  (and (starts-with a 'if) (starts-with b 'if)
       (equal (second a) (second b))))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;; test case
; (combine-rules
;  '(let ((xl (exp-lhs x))) (if (eql xl '1) '0))
;  '(let ((xl (exp-lhs x)))
;    (if (op? x '^)
;        (let ((xll (exp-lhs xl))
;              (xlr (exp-rhs xl)))
;          (if (eql xll 'e) xlr)))))
; =>
; (LET ((XL (EXP-LHS X)))
;   (OR (IF (EQL XL '1) '0)
;       (IF (OP? X '^)
;           (LET ((XLL (EXP-LHS XL)) (XLR (EXP-RHS XL)))
;             (IF (EQL XLL 'E) XLR)))))

(defun compile-all-rules-indexed (rules)
  "Compile a seperate fn for each operator, and store it
as the simp-fn of the operator."
  (index-rules rules)
  (let ((all-ops (delete-duplicates (mapcar #'main-op rules))))
    (mapc #'compile-rule-set all-ops)))

(defun simp-fn (op) (get op 'simp-fn))
(defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn))


;;;finally we don't need to use rule-based-translator.
(defun simplify-exp (exp)
  "Simplify by doing arithmetic, or by using the simp function
supplied for this operator. Do not use rules of any kind."
  (cond ((simplify-by-fn exp))
        ((evaluable? exp) (eval exp))
        (t exp)))

(defun simplify-by-fn (exp)
  "If there is a simplification fn for this exp,
  and if applying it gives a non-null result,
  then simplify the result and return that."
  (let* ((fn (simp-fn (exp-op exp)))
         (result (if fn (funcall fn exp))))
    (if (null result)
        nil
        (simplify result))))









