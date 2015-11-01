;;;; Be concise
;;; more concise
(defun vector-add (x y)
  "Element-wise add of two vectors"
  (mapcar #'+ x y))

(defun matrix-add (A B)
  "Element-wise add of two matrices (list of lists)"
  (mapcar #'vector-add A B))

;;; Or use generic functions
(defun add (&rest args)
  "Generic addition"
  (if (null args)
      0
      (reduce #'binary-add args)))

(defmethod binary-add ((x number) (y number))
  (+ x y))

(defmethod binary-add ((x sequence) (y sequence))
  (map (type-of x) #'binary-add x y))


;;;; avoid re-initialize
;;; defvar and pushnew
(defvar *options* '())
(defun add-option (x) (pushnew x *options*))

;;; sometimes we do want to re-initialize upoon re-load
(defparameter *use-experimental-mode* nil
  "Set this to T when experimental code works.")


;;;; eval-when
(eval-when (:execute) ...)
;;; equals
(eval-when (:compile-toplevel) ...)
;;; plus
(eval-when (:load-toplevel) ...)


;;;; avoid code duplication
(do ((x (f (g (h)))
        (f (g (h)))))
    (nil) ...)

;;; into
(flet ((fgh () (f (g (h)))))
  (do ((x (fgh) (fgh))) (nil) ...))

;;; in macro
(defmacro handler-case-if (test form &rest cases)
  (let ((do-it (gensym "DO-IT")))
    `(flet ((,do-it () ,form))
      (if test
          (handler-case (,do-it) ,@cases)
          (,do-it)))))

;;;; Using the conditioning system
;;; error, cerror
;;; warn
;;; handler-case
;;; with-simple-restart
;;; unwind-protect

;;; warn
(defvar *word* '?? "The word we are currently working on.")

(defun lex-warn (format-str &rest args)
  "Lexical warning; like warn, but first tells what word caused the warning."
  (warn "For word ~a: ~?" *word* format-str args))

;;; handle specifc errors
(defun op (exp)
  (first exp))

(defun args (exp)
  (rest exp))

(defun eval-exp (exp)
  "If possible evaluate this exp; otherwise return it."
  ;; Guard against errors in evaluating exp
  (handler-case
      (if (and (fboundp (op exp))
               (every #'is-constant (args exp)))
          (eval exp)
          exp)
    (arithmetic-error () exp)))

;;; provide restarts
(defun top-level (&key (prompt "=>") (read #'read)
                       (eval #'eval) (print #'print))
  "A read-eval-print loop."
  (with-simple-restart
      (abort "Exit out of the top level.")
    (loop
        (with-simple-restart
            (abort "Return to top level loop.")
          (format t "~&~a" prompt)
          (funcall print (funcall eval (funcall read)))))))


;;;; unwind-protect
;;; possibly bad (with multi-tasking)
(catch 'robot-op
  (unwind-protect
       (progn (turn-on-motor)
              (manipulate))
    (turn-off-motor)))

;;; good (safer)
(catch 'robot-op
  (let ((status (motor-status motor)))
    (unwind-protect
         (progn (turn-on-motor motor)
                (manipulate))
      (when (motor-on? motor)
        (turn-off-motor motor))
      (setf (motor-status motor) status))))


;;;; Data abstraction
;;; pretty good
(defun event ()
  ((starting-time :type integer)
   (location :type location)
   (duration :type integer :initform 0)))

;;; better
(deftype time () "Time in seconds" 'integer)

(defconstant +the-dawn-of-time+ 0
  "Midnight, January 1, 1900")

(defclass event ()
  ((starting-time :type time :initform +the-dawn-of-time+)
   (location :type location)
   (duration :type time :initform 0)))


;;;; Use abstract data types
;;; bad
(if (eval (cadar rules)) ...)

;;; better
(declaim (inline rule-antecedent))
(defun rule-antecedent (rule) (second rule))

(if (holds? (rule-antecedent (first rules))) ...)

;;; usually best
(defstruct rule
  name antecedent consequence)
;; or
(defclass rule ()
  (name antecedent consequence))


;;;; Function abstraction
;;; decomposition
;;; bad
(defun least-common-superclass (instances)
  (let ((candidates
         (reduce #'intersection
                 (mapcar #'(lambda (instance)
                             (clos:class-precedence-list
                              (class-of instance)))
                         instances)))
        (best-candidate (find-class t)))
    (mapl
     #'(lambda (candidates)
         (let ((current-candidate (first candidates))
               (remaining-candidates (rest candidates)))
           (when (and (subtypep current-candidate
                                best-candidate)
                      (every
                       #'(lambda (remaining-candidates)
                           (subtypep current-candidate
                                     remaining-candidates))
                       remaining-candidates))
             (setf best-candidate current-candidate))))
     candidates)
    best-candidate))

;;; very good
;;; but low reusability
(defun least-common-superclass (instances)
  (reduce #'more-specific-class
          (common-superclasses instances)
          :initial-value (find-class 't)))

(defun common-superclasses (instances)
  (reduce #'intersection
          (superclass-lists instances)))

(defun superclass-lists (instances)
  (loop for instance in instances
        collect (clos:class-precedence-list
                 (class-of instance))))

(defun more-specific-class (class1 class2)
  (if (subtypep class2 class1) class2 class1))


;;; equally good
;;; more reusable
(defun least-common-superclass (instances)
  "Find a least class that all instances belong to."
  (least-upper-bound (mapcar #'class-of instances)
                     #'clos:class-precedence-list
                     #'subtypep))

(defun least-upper-bound (elements supers sub?)
  "Element of lattice that is a super of all elements."
  (reduce #'(lambda (x y)
              (binary-least-upper-bound x y supers sub?))
          elements))

(defun binary-least-upper-bound (x y supers sub?)
  "Least upper bound of two elements."
  (reduce-if sub? (intersection (funcall supers x)
                                (funcall supers y))))

(defun reduce-if (pred sequence)
  "E.g. (reduce-if #'> numbers) computes maximum"
  (reduce #'(lambda (x y) (if (funcall pred x y) x y))
          sequence))


;;;; Rules of English translation
;;; 1, description of the requirement
(defun count-swarms (monster-names)
  "Count the swarms in a list of monster names."
  (count-if #'swarm-p monster-names :key #'get-object)
  ;; or
  (count 'swarm monster-names :key #'get-object-type)
  ;; or
  (loop for name in monster-names
        count (swarm-p (get-object name))))

;;;; Efficiency
;;; good
(defun find-character (char string)
  "See if the character appears in the string."
  (declare (character char) (simple-string string))
  (loop for ch across string
        when (eql ch char) return ch))


;;;; Control abstraction
;;; bad
(defun any (1st)
  (cond ((null 1st) nil)
        ((car 1st) t)
        (t (any (cdr 1st)))))

;;; better
(defun any (list)
  "Return true if any member of list is true."
  (some #'not-null list)
  ;; or
  (find-if-not #'null 1st)
  ;; or
  (loop for x in list thereis x)
  ;; or explicitly
  (do ((list list (rest list)))
      ((null list) nil)
    (when (first list)
      (return t))))

;;; best
;;; Don't call any at all!
(some #'not-null list)


;;; simple iteration
;;; bad
(loop
    (setf *word* (pop *sentence*))      ;get the next word
    (cond
      ;; if no more words then return instantiated CD form
      ;; which is stored in the variable *CONCEPT*
      ((null *word*)
       (return (remove-variables (var-value '*concept*))))
      (t (format t "~%~%Processing ~A" *word*)
         (load-def)                     ; look up requests under
         								; this word
         (run-stack))))                 ; fire requests

;;; good (conventional, concise, explicit)
(mapc #'process-word sentence)
(remove-variables (var-value '*concept*))

(defun process-word (word)
  (format t "~2%Processing ~A" word)
  (load-def word)
  (run-stack))


;;; use tools/libraries
;;; good
(defun isa-test (sub super max-depth)
  "Test if SUB is linked to SUPER by a chain of ISA links shorter than max-depth."
  (and (>= max-depth 0)
       (or (eq sub super)
           (some #'(lambda (parent)
                     (isa-test parent super
                               (- max-depth 1)))
                 (get sub 'isa)))))

;;; also good: use tools
(defun isa-test (sub super max-depth)
  (depth-first-search :start sub :goal (is super)
                      :successors #'get-isa
                      :max-depth max-depth))


;;;; avoid complicated lambda expressions
;;; Find the sum of the squares of the odd numbers in a list of integers:
;;; all good
(reduce #'+ numbers
        :key #'(lambda (x) (if (oddp x) (* x x) 0)))

(flet ((square-odd (x) (if (oddp x) (* x x) 0)))
  (reduce #'+ numbers :key #'square-odd))

(loop for x in list
      when (oddp x) sum (* x x))

(collect-sum (choose-if #'oddp numbers))

;;; also consider
;; introduce read macro:
(reduce #'+ numbers :key #L(if (oddp _) (* _ _) 0))

;; generate intersection garbage:
(reduce #'+ (remove #'evenp (mapcar #'square numbers)))


;;;; Syntactic Abstraction
;;; memo, saving duplicate work
;;; examples:
;;; (defun-memo f (x)
;;;   (complex-computation x))   	;; eql table keyed on x
;;; (defun-memo (f :test #'eq) (x)	;; eq table keyed on x
;;;   (complex-computation x))
;;; (defun-memo g (x y z)
;;;   (another-computation x y z))	;; equal table keyed on (x y . z)
;;; (defun-memo (h :key-exp x) (x &optional debug?)
;;;   ...)							;; eql table keyed on x
(defmacro defun-memo (fn-name-and-options (&rest args)
                                          &body body)
  ;; Documentation string on previous page
  (let ((vars (arglist-vars args)))
    (flet ((gen-body (fn-name &key (test '#'equal)
                              size key-exp)
             `(eval-when (load eval compile)
               (setf (get ',fn-name 'memoize-table)
                (make-hash-table :test ,test
                 ,@(when size `(:size ,size))))
               (defun ,fn-name ,args
                 (gethash-or-set-default
                  ,key-exp
                  (get ',fn-name 'memoize-table)
                  (progn ,@body))))))
      ;; Body of the macro:
      (cond ((consp fn-name-and-options)
             ;; Use user-supplied keywords, if any
             (apply #'gen-body fn-name-and-options))
            ((and (= (length vars) 1)
                  (not (member '&rest args)))
             ;; Use eql table if it seems reasonable
             (gen-body fn-name-and-options :test '#'eql
                       :key-exp (first vars)))
            (t ; Otherwise use qual table on all args
             (gen-body fn-name-and-options :test '#'equal
                       :key-exp `(list* ,@vars)))))))


;;; with-gensyms
(defmacro with-gensyms (symbols body)
  "Replace the given symbols with gensym-ed versions,
everywhere in body. Useful for macros."
  ;; Does this everywhere, not just for "variables"
  (sublis (mapcar #'(lambda (sym)
                      (cons sym (gensym (string sym))))
                  symbols)
          body))


;;; gethash-or-set-default
(defmacro gethash-or-set-default (key table default)
  "Get the value from table, or set it to the default.
Doesn't evaluate the default unless needed."
  (with-gensyms (keyvar tabvar val found-p)
    `(let ((keyvar ,key)
           (tabvar ,table))
      (multiple-value-bind (val found-p)
          (gethash keyvar tabvar)
        (if found-p
            val
            (setf (gethash keyvar tabvar)
                  ,default))))))

;;; macros for control structures
;;; good: fills a hole in orthogonality of CL
(defmacro dovector ((var vector &key (start 0) end)
                    &body body)
  "Do body with var bound to each element of vector.
You can specify a subrange of the vector."
  `(block nil
    (map-vector #'(lambda (,var) ,@body)
     ,vector :start start :end end)))

(defun map-vector (fun vector &key (start 0) end)
  "Call fn on each element of vector within a range."
  (loop for i from start below (or end (length vector))
        do (funcall fn (aref vector-var index))))


;;;; comments
;;; notational tricks: parens in column 0
(defun factorial (n)
  "Compute the factorial of an integer.
\(don't worry about non-integer args)."
  (if (= n 0) 1
      (* n (factorial (- n 1)))))

;;; multi-line strings
;;; using real-time evaluation
(defun find-subject-line (message-header-string)
  (search #.(format nil "~%Subject:") message-header-string))

;;; this permits you to indent a fixed amount easily:
(format t "~&This is a long string.~
		   ~% This is more of that string, indent by one.")

