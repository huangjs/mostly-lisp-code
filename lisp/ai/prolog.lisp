;;; (load "unification")

;;; Prolog interpreter
;;; clause := (head . body)
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;;; index the clause
(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")

(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))

;;; add a new clause
(defmacro <- (&body clause)
  "Add a clause to the data base."
  `(add-clause ',clause))

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicats."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable? pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))

(deftest test-add-clause ()
  (show
    (<- (likes Kim Robin))
    (<- (likes Sandy Lee))
    (<- (likes Sandy kim))
    (<- (likes Robin cats))
    (<- (likes Sandy ?x) (likes ?x cats))
    (<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
    (<- (likes ?x ?x))
    )
  (run
    (format t "~%*db-predicates* are: ~&~A~%" *db-predicates*)
    (format t "~%clause of predicate likes are: ~&~A~%"
            (get-clauses 'likes))
    ))

;;; remove a predicate
(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

(deftest test-clear-predicate ()
  (show
    (clear-predicate 'likes)
    )
  (run
    (format t "~%clauses of predicate likes are: ~&~A~%"
            (get-clauses 'likes))
    ))
  
;;; prove a goal
;;; magic!!
(defun prove (goal bindings)
  "Return a list of possible solutions to goal."
  (mapcan #'(lambda (clause)            ;because clauses are ((clause a) (clause b)), so mapcan
              (let ((new-clause (rename-variables clause)))
                (prove-all (clause-body new-clause)
                           (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal))))

(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) (list bindings))  ;fact or recursive stop
        (t (mapcan #'(lambda (goal1-solution)
                       (prove-all (rest goals) goal1-solution))
                   (prove (first goals) bindings)))))

(deftest test-prove ()
  ;; run test-add-clause first
  (show
    (prove '(likes Kim Robin) no-bindings)
    (prove '(likes Sandy ?who) no-bindings)
    ))

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var)
                      (cons var (gensym (string var))))
                  (variables-in x))
          x))

(deftest test-rename-variables ()
  (show
    (rename-variables '((likes Sandy ?who) . ((likes ?who cats))))
    (rename-variables '((likes ?x ?x)))
    ))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable? exp))

(deftest test-variables-in ()
  (show
    (variables-in '((likes Sandy ?who) . ((likes ?who cats))))
    (variables-in '((likes ?x ?x)))
    ))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicates,
with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree)
                                found-so-far))))

;;; query interface(macro)
(defmacro ?- (&body goals)
  `(prove-all ',goals no-bindings))

;;; test
(deftest test-prolog1 ()
  (show
    (<- (likes Kim Robin))
    (<- (likes Sandy Lee))
    (<- (likes Sandy kim))
    (<- (likes Robin cats))
    (<- (likes Sandy ?x) (likes ?x cats))
    (<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
    (<- (likes ?x ?x))
    ))

(deftest test-prolog2 ()
  (show
    (?- (likes Sandy ?who))
    ))

;;; clean up the output
(defmacro ?- (&body goals)
  `(top-level-prove ',goals))

(defun top-level-prove (goals)
  "Prove the goals and print variables readably."
  (show-prolog-solutions
   (variables-in goals)
   (prove-all goals no-bindings)))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions."
  ;; a REPL
  (if (null solutions)                  ; coz fail = nil
      (format t "~&No.")
      (mapc #'(lambda (solution)        ; has solution then ...
                (show-prolog-vars vars solution))
            solutions))
  (values))

(defun show-prolog-vars (vars bindings)
  "Print each variable with its binding."
  ;; goal already has solution.
  (if (null vars)
      (format t "~&YES")                
      (dolist (var vars)
        (format t "~&~A = ~A" var
                (subst-bindings bindings var))))
  (princ ";"))

;;; test
(deftest test-prolog3 ()
  (show
    (?- (likes Sandy ?who))
    (?- (likes ?who Sandy))
    (?- (likes Robin Lee))
    (?- (likes Kim Robin))
    ))
      
(deftest test-prolog4 ()
  (show
    (<- (member ?item (?item . ?rest)))
    (<- (member ?item (?x . ?rest))
        (member ?item ?rest))
    (?- (member 2 (1 2 3)))
    (?- (member 2 (1 2 3 2 1)))
    ))

