;;;; L-99 problems

;;; P01: #'last

;;; P02:
(defun my-but-last (list)
  "Find the last but one box of a list.
Example:
* (my-but-last '(a b c d))
=> (C D)"
  (last list 2))

;;; P03: #'nth

;;; P04: #'length

;;; P05: #'reverse

;;; P06:
(defun palindromep (list)
  "Find out whether a list is a palindrome.
A palindrome can be read forward or backward; e.g. (x a m a x)."
  (equal list (reverse list)))

;;; P07:
(defun flatten (list &optional accumulator)
  "Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:
* (my-flatten '(a (b (c d) e)))
=> (A B C D E)"
  (cond ((null list) accumulator)
        ((atom list) (cons list accumulator))
        (t (flatten (first list)
                    (flatten (rest list) accumulator)))))

;;; P08: #'remove-duplicates

;;; P09:
(defun pack (list &key (test #'eql))
  "Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:
* (pack '(a a a a b c c a a d e e e e))
=> (A A A A) (B) (C C) (A A) (D) (E E E E))"
  ;; put the same
  (labels ((reverse-pack (list &optional package accumulator)
	     (cond ((null list)
		    (unless (null package)
		      (push package accumulator))
		    accumulator)
		   ((funcall test (first list) (first package))
		    (reverse-pack (rest list)
				  (cons (first list) package)
				  accumulator))
		   (t (reverse-pack (rest list)
				    (list (first list))
				    (cons package accumulator))))))
    (rest (nreverse (reverse-pack list)))))

;;; P10:
(defun encode (list &key (test #'eql))
  "Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:
* (encode '(a a a a b c c a a d e e e e))
=> (4 A) (1 B) (2 C) (2 A) (1 D)(4 E))"
  (labels ((reverse-encode (list &optional package accumulator)
	     (cond ((null list)
		    (unless (null package)
		      (push package accumulator))
		    accumulator)
		   ((funcall test (first list) (second package))
		    (reverse-encode (rest list)
				    (progn
				      (incf (first package))
				      package)
				    accumulator))
		   (t (reverse-encode (rest list)
				      (list 1 (first list))
				      (cons package accumulator))))))
    (rest (nreverse (reverse-encode list)))))

;;; P11:
;;; lists are not allowed as list elements
(defun encode-modified (list &key (test #'eql))
  "Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:
* (encode-modified '(a a a a b c c a a d e e e e))
=> ((4 A) B (2 C) (2 A) D (4 E))" 
  (labels ((reverse-encode (list &optional package accumulator)
	     (cond ((null list)
		    (unless (null package)
		      (push package accumulator))
		    accumulator)
		   ((funcall test (first list) (second package))
		    (reverse-encode (rest list)
				    (progn
				      (incf (first package))
				      package)
				    accumulator))
		   (t (reverse-encode (rest list)
				      (list 1 (first list))
				      (if (= (first package) 1)
					  (cons (second package) accumulator)
					  (cons package accumulator)))))))
    (rest (nreverse (if (null list)
			(reverse-encode list)
			(reverse-encode (rest list)
					(list 1 (first list))
					(list nil)))))))

;;; P12:
(defun decode (list)
  "Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P11. Construct its uncompressed version. "
  (let ((result nil))
    (labels ((expand (amount element)
	       (dotimes (i amount)
		 (declare (ignorable i))
		 (push element result))))
      (dolist (package list)
	(typecase package
	  (list (apply #'expand package))
	  (t (expand 1 package))))
      (nreverse result))))

;;; P13: P11

;;; P14: #'copy-list or #'copy-tree

;;; P15:
(defun repli (list times)
  "Replicate the elements of a list a given number of times.
Example:
* (repli '(a b c) 3)
=> (A A A B B B C C C)"
  (let ((result nil))
    (dolist (element list)
      (dotimes (i times)
	(declare (ignorable i))
	(push element result)))
    (nreverse result)))

;;; P16:
(defun drop (list circle)
  "Drop every N'th element from a list.
Example:
* (drop '(a b c d e f g h i k) 3)
=> (A B D E G H K)"
  (let ((result nil))
    (loop :for element :in list
       :with counter = 1
       :do (progn
	     (if (= counter circle)
		 (setf counter 1)
		 (progn
		   (push element result)
		   (incf counter)))))
    (nreverse result)))

;;; P17:
(defun split (list position)
  "Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

Example:
* (split '(a b c d e f g h i k) 3)
=> ( (A B C) (D E F G H I K))"
  (labels ((split-helper (part1 part2 position)
	     (cond ((null part2)
		    (list (nreverse part1)))
		   ((<= position 0)
		    (list (nreverse part1) part2))
		   (t (split-helper (push (first part2) part1)
				    (rest part2)
				    (decf position))))))
    (split-helper nil list position)))

;;; P17 destructive version: n times faster
(defun nsplit (list position)
  "(Destructive version of split).
Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

Example:
* (nsplit '(a b c d e f g h i k) 3)
=> ( (A B C) (D E F G H I K))"
  (let ((left-tail-cursor list)
	(right-head-cursor (rest list)))
    (cond ((= position 0)
	   (list nil list))
	  ((>= position (length list))
	   (list list))
	  (t (progn
	       (dotimes (i (1- position))
		 (declare (ignorable i))
		 (setf left-tail-cursor (rest left-tail-cursor))
		 (setf right-head-cursor (rest right-head-cursor)))
	       (setf (cdr left-tail-cursor) nil)
	       (list list right-head-cursor))))))

;;; P18:
(defun slice (list start end)
  "Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

Example:
* (slice '(a b c d e f g h i k) 3 7)
=> (C D E F G)"
  (when (<= start 0)
    (setf start 1))
  (when (> end (length list))
    (setf end (length list)))
  (subseq list (1- start) end))

;;; P18 destructive version: n times faster
;;; FIXME!! buggy on boundary conditions
(defun nslice (list start end)
  "(Destructive version of slice). 
Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

Example:
* (nslice '(a b c d e f g h i k) 3 7)
=> (C D E F G)"
  (let ((length (length list))
	(head list)
	(tail list))
    (cond ((or (>= start length)
	       (< end start))
	   (return-from nslice nil))
	  (t (dotimes (i (1- start))
	       (declare (ignorable i))
	       (setf head (rest head)))))
    (if (>= end length)
	head
	(progn
	  (setf tail head)
	  (dotimes (i (- end start))
	    (declare (ignorable i))
	    (setf tail (rest tail)))
	  (setf (cdr tail) nil)
	  head))))

;;; P18: another implementation
(defun nslice2 (list start end)
  (when (<= start 0)
    (setf start 1))
  (destructuring-bind (left right)
      (nsplit list (1- start))
    (declare (ignorable left))
    (if (>= end (length list))
	right
	(destructuring-bind (middle right)
	    (nsplit right (1+ (- end start)))
	  (declare (ignorable right))
	  middle))))

;;; P19:
(defun rotate (list position)
  "Rotate a list N places to the left.
Examples:
* (rotate '(a b c d e f g h) 3)
=> (D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
=> (G H A B C D E F)"
  (when (minusp position)
    (setf position (+ (length list) position)))
  (destructuring-bind (left right)
      (split list position)
    (append right left)))

;;; P19 destructive version
(defun nrotate (list position)
  "(Destructive version of rotate). 
Rotate a list N places to the left.
Examples:
* (nrotate '(a b c d e f g h) 3)
=> (D E F G H A B C)

* (nrotate '(a b c d e f g h) -2)
=> (G H A B C D E F)"
  (when (minusp position)
    (setf position (+ (length list) position)))
  (destructuring-bind (left right)
      (nsplit list position)
    (append right left)))

;;; P20:
(defun remove-at (list position)
  "Remove the K'th element from a list.
Example:
* (remove-at '(a b c d) 2)
=> (A C D)"
  (when (or (<= position 0)
	    (> position (length list)))
    (return-from remove-at list))
  (destructuring-bind (left right)
      (split list (1- position))
    (append left (rest right))))

;;; P20 destructive version
(defun nremove-at (list position)
  "(Destructive version of remove-at).
Remove the K'th element from a list.
Example:
* (nremove-at '(a b c d) 2)
=> (A C D)"
  (when (or (<= position 0)
	    (> position (length list)))
    (return-from nremove-at list))
  (destructuring-bind (left right)
      (nsplit list (1- position))
    (append left (rest right))))

;;; P21:
(defun insert-at (value list position)
  " Insert an element at a given position into a list.
Example:
* (insert-at 'alfa '(a b c d) 2)
=> (A ALFA B C D)"
  (destructuring-bind (left right)
      (split list (1- position))
    (append left (list value) right)))

;;; P21 destructive version
(defun ninsert-at (value list position)
  "(Destructive version of insert-at).
 Insert an element at a given position into a list.
Example:
* (ninsert-at 'alfa '(a b c d) 2)
=> (A ALFA B C D)"
  (destructuring-bind (left right)
      (nsplit list (1- position))
    (append left (list value) right)))

;;; P22:
(defun range (start end)
  "Create a list containing all integers within a given range.
Example:
* (range 4 9)
=> (4 5 6 7 8 9)"
  (let ((result nil))
    (loop :for i :from start :to end
       :do (push i result))
    (nreverse result)))

;;; P23:
(defun rnd-select (list amount)
  "Extract a given number of randomly selected elements from a list.
The selected items shall be returned in a list.
Example:
* (rnd-select '(a b c d e f g h) 3)
=> (E D A)"
  (labels ((shuffle! (l &optional (start 0) stop)
	     (let ((len (length l)))
	       (unless stop
		 (setf stop len))
	       (do ((rest (nthcdr start l) (cdr rest))
		    (posn start (1+ posn)))
		   ((or (>= posn stop)
			(null rest))
		    l)
		 (let ((swaposn (random (- len posn))))
		   (when (not (zerop swaposn))
		     (rotatef (car (nthcdr swaposn rest)) (car rest))))))))
    (let ((list (copy-list list)))
      (subseq (shuffle! list 0 amount) 0 amount))))

;;; P24:
(defun rnd-select-from-range (amount ceiling)
  "Lotto: Draw N different random numbers from the set 1..M.
The selected numbers shall be returned in a list.
Example:
* (rnd-select 6 49)
=> (23 1 17 33 21 37)"
  (rnd-select (range 1 ceiling) amount))

;;; P25
(defun rnd-permu (list)
  "Generate a random permutation of the elements of a list.
Example:
* (rnd-permu '(a b c d e f))
=> (B A D C E F)"
  (rnd-select list (length list)))


