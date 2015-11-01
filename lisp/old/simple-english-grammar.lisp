(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article noun))
    (verb-phrase -> (verb noun-phrase))
    (article -> the a)
    (noun -> man ball woman table)
    (verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is
*simple-grammar*, but we can switch to other grammars.")

;;; accessors
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))


;;; rule processor
(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond ((listp phrase)
         (mapcan #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun one-of (set)
  "Pick one element of a set, and make a list of it."
  (list (random-elt set)))

;;; avoid calling rewrites twice
(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond ((listp phrase)
         (mapcan #'generate phrase))
        (t
         (let ((choices (rewrites phrase)))
           (cond ((null choices)
                  (list phrase))
                 (t (generate (random-elt (rewrites phrase)))))))))

;;;; generate with non-terminal predicates
(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond ((listp phrase)
         (mapcan #'generate phrase))
        ((terminal-p phrase)
         (list phrase))
        (t (generate (random-elt (rewrites phrase))))))

(defun terminal-p (category)
  "True if this is a category in the grammar."
  (null (rewrites category)))

;;; a more complicated grammar
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article adj* noun pp*) (name) (pronoun))
    (verb-phrase -> (verb noun-phrase pp*))
    (pp* -> () (pp pp*))
    (adj* -> () (adj adj*))
    (pp -> (prep noun-phrase))
    ;; primitives
    (prep -> to in by with on)
    (adj -> big little blue green adiabatic)
    (article -> the a)
    (name -> Pat Kim Lee Terry Robin)
    (noun -> man ball woman table)
    (verb -> hit took saw liked)
    (pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

;;; a complete syntax of a sentence.
(defun generate-tree (phrase)
  "Generate a random sentence or phrase.
with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

;;; generate all possible sentences
(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mapcan #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))      ; why (list (list phrase))? see examples in combine-all.

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
e.g., (combine-all '((a) (b)) '((1) (2))) -> ((a 1) (b 1) (a 2) (b 2))."
  (mapcan #'(lambda (y)
              (mapcar #'(lambda (x) (append x y)) xlist))
          ylist))


;;; abstraction of combin-all
;;; the Cartesian product.
(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) pairs."
  (mapcan #'(lambda (y)
              (mapcar #'(lambda (x) (funcall fn x y))
                      xlist))
          ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x."
  (cross-product #'append xlist ylist))
