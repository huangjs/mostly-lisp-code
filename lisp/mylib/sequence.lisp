;;; (load "essential")

(alias filter remove-if-not)

(alias accumulate reduce)

(alias find-all-if remove-if-not)

(defun find-all (item sequence &rest keyword-args
                      &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
 according to the keywords. Doesn'nt alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

;;; permutations
(defun permute (l)
  "List all the permutation of the input list.
Repetitions are allowed."
  (if (null l)
      '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permute
                           (remove e l :count 1 :test #'eq))))
              l)))
                           
(defun del-repeat (seq)
  "Turn a sequence into a set. Eliminate repetitions."
  ;; Sequence -> List
  (if (null seq)
      nil
      (progn
        (let ((e (elt seq 0)))                ;first element
          (cons e (del-repeat (remove e seq :test #'equal)))))))

;;; only slightly faster in sbcl.
(defun del-repeat-iter (seq result)
  "Turn a sequence into a set. Eliminate repetitions."
  ;; Sequence -> List
  (if (null seq)
      result
      (progn
        (let ((e (elt seq 0)))                ;first element
          (push e result)
          (del-repeat-iter (remove e seq :test #'equal) result)))))

(defun permute-no-repeat (l)
  "List all the permutation of the input list.
No repetitons."
  (del-repeat-iter (permute l) nil))


;;; flatten a list
(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (flet ((mklist (x)
           ;; Return x if it is a list, otherwise (x).
           (if (listp x)
               x
               (list x))))
    (mapcan #'mklist the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

;;; prepend
(defun prepend (x y)
  "Prepend y to start of x"
  (append y x))


;;; distribute an element with a set
;;; more general than cartesian-product
(defun distribute (e y)
  "Association of an element with a set."
  ;; make it robust
  (cond ((and (listp e)
              (listp y))
         (cartesian-product e y))
        ((listp e)
         (loop for a in e collect
               (list a y)))             ; preserve association order
        ((atom y)
         (list e y))
        (t 
         (loop for a in y collect
               (list e a)))))
  
;;; cartesian product of 2 sets
(defun cartesian-product (x y)
  "Cartesian product of set x and set y."
  (let ((results nil))
    (loop for a in x do
          (loop for b in y do
                (push (list a b) results)))
    (reverse results)))

(alias direct-product cartesian-product)
(alias d-product cartesian-product)


;;; deep copy of a list
(defun deep-copy (object)
  (cond ((consp object)
         (cons (deep-copy (car object))
               (deep-copy (cdr object))))
        (t object)))

