;;; A uniform data base, using lisp syntax.

;;; Facts
;;; e.g. (population SF 750000)
;;; (capital Sacramento CA)
;;; we use <- macro to distinguish from lisp function calls
;;; (<- (likes Kim Robin))
;;; (<- (likes ?who Lee))

;;; Rules
;;; e.g. (<- (likes Sandy ?x) (likes ?x cats))
;;; This means: for any x sandy likes x if x likes cats.
;;; or, If you want to show that Sandy likes some x, one way to do it is to show that x likes cats.
;;; This is called a BACKWARD-CHAINING interpretation.
;;; therefore <- means logic implication.

;;; Prolog use backward chaining exclusively,
;;; many Expert system use forward chaining exclusively or both.

;;; formal syntax of rules
;;; (<- head body...)
;;; a fact is a rule that has no body. that is, a fact is true no matter what.
;;; A clause asserts that the haed is true only if ALL the goals in the body are true.

;;; another example
;;; (<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
;;; This can be read as:
;;; For any x, deduce that kim likes x
;;;  if it can be proved that x likes Lee and x likes Kim.


;;; Unifivation of Logic variables
;;; (unify '(?x + 1) '(2 + ?y)) => ((?y . 1) (?x . 2))
;;; and once binded to a value, a logic variable cannot change its value.
;;; (unify '(f ?x) '(f ?y)) => ((?x . ?y))

;;; UNIFIER, which shows the structure that results from unifying two structures.
;;; use the relations by unify and make deduction.
;;; e.g. (unifier '(?a + ?a = 0) '(?x + ?y = ?y)) => (0 + 0 = 0)

;;; recall pattern matching.

; (defun pat-match (pattern input &optional (bindings no-bindings))
;   "Match pattern against input in the context of the bindings."
;   (cond ((eq bindings fail) fail)
;         ((variable? pattern)
;          (match-variable pattern input bindings))
;         ((eql pattern input) bindings)
;         ((segment-pattern? pattern)
;          (segment-matcher pattern input bindings))
;         ((single-pattern? pattern)
;          (single-matcher pattern input bindings))
;         ((and (consp pattern) (consp input))
;          (pat-match (rest pattern) (rest input)
;                     (pat-match (first pattern) (first input)
;                                bindings)))
;         (t fail)))

; (defun match-variable (var input bindings)
;   "Does VAR match input? Uses (or updates) and returns bindings."
;   (let ((binding (get-binding var bindings)))
;     (cond ((not binding) (extend-bindings var input bindings))
;           ((equal input (binding-val binding)) bindings)
;           (t fail))))

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)            ;*** 
        ((variable? x) (unify-variable x y bindings))
        ((variable? y) (unify-variable y x bindings)) ;***
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))  ;***
         (unify var (lookup x bindings) bindings))  ;***
        (t (extend-bindings var x bindings))))

;;; problem
;;; (unify '?x '(f ?x)) => ((?x f ?x)) means ?x is bounded to (f ?x)
;;; this represents a circular, infinite unification., very tricky

;;; solution: simply just to ban them.
;;; modify the unifier so that it fails whenever there is a circular occurs.
;;; NOTE: in practice, circular rarely appears.
;;; and it adds to a lot of computational complexity, most Prolog systems have ignored it.

;;; a var here is provided to allow the user to turn occurs checking on or off.
(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))  ;***
         (unify var (lookup x bindings) bindings))  ;***
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable? x) (get-binding x bindings))  ;circular in variable bindings
         (occurs-check var (lookup x bindings) bindings))
        ((consp x)                      ;circular in a structure containing the variable to match
         (or (occurs-check var (first x) bindings)
             (occurs-check var (rest x) bindings)))
        (t nil)))

;;; test of unify
(deftest test-unify ()
  (show 
    (unify '((?a * ?x ^ 2) + (?b * ?x) + ?c)
           '(?z + (4 * 5) + 3))         ;=> ((?c .3) (?x . 5) (?b . 4) (?z ?a * ?x ^ 2))
    (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y)))  ;=> nil
    (unify '(?x ?y) '((f ?x) (f ?y)))   ;=> nil
    (unify '?x '(f ?x))                 ;=> nil
    (unify '(?x + 1) '(2 + ?y))         ;=> ((?y . 1) (?x . 2))
    ))


;;; how to use unify?
;;; sublis won't work any more, because variables can be bound to other variables
;;; which are in turn bound to expressions.
(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(defun unifier (x y)
  "Return somehting that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))

;;; test of unifier
(deftest test-unifier ()
  (show
    (unifier '(?x ?y a) '(?y ?x ?x))        ;=> (A A A)
    (unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
             '(?z + (4 * 5) + 3))           ;=> ((?A * 5 ^ 2) + (4 * 5) + 3)
    ))

