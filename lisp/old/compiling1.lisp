;;; chapter 2
;;; natural language rule compiler
(defun compile-rule (rule)
  "Translate a grammar rule into a LISP function definition."
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
      ,(cond ((every #'atom rhs) `(one-of ',rhs))  ;(DEFUN NOUN () (ONE-OF '(MAN BALL WOMAN TABLE))
             ((length=1 rhs) (build-code (first rhs)))
             (t `(case (random ,(length rhs))
                  ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
  "Return a list of case-clauses."
;   (DEFUN NOUN-PHRASE ()
;   (CASE (RANDOM 3)
;     (0 (APPEND (ARTICLE) (ADJ*) (NOUN) (PP*)))
;     (1 (NAME))
;     (2 (PRONOUN)))
  (when choices
    (cons (list number (build-code (first choices)))
          (build-cases (+ number 1) (rest choices)))))

(defun build-code (choice)
  "Append together multiple constituents"
  ;;(DEFUN VERB-PHRASE () (APPEND (VERB) (NOUN-PHRASE))
  (cond ((null choice) nil)
        ((atom choice) (list choice))
        ((length=1 choice) choice)
        (t `(append ,@(mapcar #'build-code choice)))))

(defun length=1 (x)
  "Is X a list of length 1?"
  (and (consp x)
       (null (rest x))))

;;; then we can use:
;;; (dolist (rule *grammar*) (compile (eval (compile-rule rule))))

;;; we make a macro for rule defining
(defmacro defrule (&rest rule)
  "Define a grammar rule"
  ;; (defrule verb -> hit took saw liked
  (compile-rule rule))


;;; problem:
;;; 1. compound noun
;;; e.g., (defrule Noun -> man ball woman table (chow chow))
;;; 2. name confliction
;;; solution: package system

