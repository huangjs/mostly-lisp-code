;;;; Low level efficiency

;;; Declaration
;;; test1
(defun sum-squares (vect)
  (let ((sum 0))
    (dotimes (i (length vect))
      (incf (sum (square (svref vect i))))))
  sum)

(defun sum-squares-optimized (vect)
  (declare (type (simple-array fixnum *) vect)
           (inline square)
           (optimize speed (safety 0)))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (length vect))
      (declare (fixnum i))
      (incf (sum (the fixnum (square (aref vect i)))))))
  sum))

;;; test2
(defun clear-m-array (array)
  (dotimes (i 1024)
    (dotimes (j 1024)
      (setf (aref array i j) 0.0))))

(defun clear-m-array-optimized (array)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array single-float (1024 1024) array)))
  (dotimes (i 1024)
    (dotimes (j 1024)
      (setf (aref array i j) 0.0))))


;;; Specific function instead of Generic function
(defun random-elt (s) (elt s (random (length s))))
(defun random-mem (l) (nth (random (length (the list l))) l))

;;; compile and disassemble
(proclaim '(optimize (speed 3) (debug 0) (safety 0) (space 0)))
(defun reg (a b c d) (list a b c d))
(defun rst (a b c &rest d) (list* a b c d))
(defun opt (&optional a b (c 1) (d (sqrt a))) (list a b c d))
(defun key (&key a b (c 1) (d (sqrt a))) (list a b c d))
(compile 'reg)
(compile 'rst)
(compile 'opt)
(compile 'key)

(time (dotimes (i 200000)
        (reg 1 2 3 4)))

;;; keyword arugments
;;; make *no-key inline result in tremendous performance up!!! (sbcl)
;;; This is not mentioned in the book paip!!!
(proclaim '(optimize (speed 3) (debug 0) (safety 0) (space 0)))
(proclaim '(inline key))
(proclaim '(inline *no-key))
(defun key (&key a b (c 1) (d (sqrt a))) (*no-key a b c d))
(defun *no-key (a b c d) (list a b c d)) 
(compile 'key)
(compile '*no-key)

;;; macro automatically define the interface to the keyword-less function:
;;; The use of defun*
(defmacro defun* (fn-name arg-list &rest body)
  "Define two functions, one an interface to a &keyword-less
version. Proclaim the interface function inline."
  (if (and (member '&key arglist)
           (not (member '&rest arg-list)))
      (let ((no-key-fn-name (symbol fn-name '*no-key))
            (args (mapcar #'first-or-self
                          (set-difference arglist lambda-list-keywords))))
        `(progn
          (proclaim '(inline ,fn-name))
          (defun ,no-key-fn-name ,args
            .,body)
          (defun ,fn-name ,arg-list
            (,no-key-fn-name .,args))))
      `(defun ,fn-name ,arg-list
        .,body)))


;;; avoid unnecessary consing
;;; O(n^2) time and space
(defun flatten (input)
  "Return a flat list of the atoms in the input.
Ex: (flatten '((a) (b (c) d))) => (a b c d)."
  (cond ((null input) nil)
        ((atom input) (list input))
        (t (append (flatten (first input))
                   (flatten (rest input))))))

;;; an improved version
(defun flatten (input &optional accumulator)
  "Return a flat list of the atoms in the input.
Ex: (flatten '((a) (b (c) d))) => (a b c d)."
  (cond ((null input) accumulator)
        ((atom input) (cons input accumulator))
        (t (flatten (first input)
                    (flatten (rest input) accumulator))))) 

;;; reuse-cons
;;; !!! not good in modern computers
(proclaim '(inline reuse-cons))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (eql x (car x-y))
           (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun remq (item list)
  "Like REMOVE, but uses EQ, and only works on lists."
  (cond ((null list) nil)
        ((eq item (first list))
         (remq item (rest list)))
        (t (reuse-cons (first list)
                       (remq item (rest list))
                       list))))

;;; unique-cons
;;; !!! save space but need benchmark!
(defparameter *uniq-atom-table* (make-hash-table :test #'eq))
(defparameter *uniq-cons-table* (make-hash-table :test #'eq))

(defun unique (exp)
  "Return a canonical representation that is EQUAL to exp.
such that (equal x y) implies (eq (unique x) (unique y))."
  (typecase exp
    (symbol exp)
    (fixnum exp)                        ;; Remove if fixnums are not eq in your LISP.
    (atom (or (gethash exp *uniq-atom-table*)
              (setf (gethash exp *uniq-atom-table*) exp)))
    (cons (ucons (car exp) (cdr exp)))))

(defun ucons (x y)
  "Return a cons s.t. (eq (ucons x y) (ucons x2 y2)) is true
whenever (equal x x2) and (equal y y2) are true."
  (let ((ux) (uy))
    (let ((car-table
           (or (gethash x *uniq-cons-table*)
               (gethash (setf ux (unique x)) *uniq-cons-table*)
               (setf (gethash ux *uniq-cons-table*)
                     (make-hash-table :test #'eq)))))
      (or (gethash y car-table)
          (gethash (setf uy (unique y)) car-table)
          (setf (gethash uy car-table)
                (cons ux uy))))))

(defun ulist (&rest args)
  "A uniquified list."
  (unique args))

(defun uappend (x y)
  "A unique list equal to (append x y)."
  (if (null x)
      (unique y)
      (ucons (first x) (uappend (rest x) y))))

;;; Using Multiple Values
;;; instead of
(defstruct point "A point in 3-D cartesian space." x y z)

(defun scale-point (k pt)
  "Multiply a point by a constant, K."
  (make-point :x (* k (point-x pt))
              :y (* k (point-y pt))
              :z (* k (point-z pt))))

;;; we use
(defun scale-point-opt (k x y z)
  "Multiply the point (x y z) by a constant, K."
  (values (* k x) (* k y) (* k z)))

;;; Resource pool
(defmacro defresource (name &key constructor (initial-copies 0)
                            (size (max initial-copies 10)))
  (let ((resource (symbol name '-resource))
        (deallocate (symbol 'deallocate- name))
        (allocate (symbol 'allocate- name)))
    `(let ((,resource (make-array ,size :fill-pointer 0)))
      (defun ,allocate ()
        "Get an element from the resource pool, or make one."
        (if (= (fill-pointer ,resource) 0)
            ,constructor
            (vector-pop ,resource)))
      (defun ,deallocate (,name)
        "Place a no-longer-needed element back in the pool."
        (vector-push-extend ,name ,resource))
      ,(if (> initial-copies 0)
           `(map #',deallocate (loop repeat ,initial-copies
                                 collect (,allocate))))
      ',name)))

;;; we could then use:
;;; (let ((b (allocate-buffer)))
;;;   ...
;;;   (process b)
;;;   ...
;;;   (deallocate-buffer b))

;;; provide a macro
;;; one more way to make the system safer is to have the DEALLOCATE
;;; function check that its argument is indeed an object of the correct type.
(defmacro with-resource ((var resource &optional protect) &rest body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (symbol 'allocate- resource))
        (deallocate (symbol 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
          (unwind-protect
               (progn (setf ,var (,allocate)) ,@body)
            (unless (null ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
          ,@body
          (,deallocate ,var)))))


;;;; Use the right data structure.

;;; symbols and keywords are fast

;;; queue
;;; tricks: car of q is the last element. cdr is the contents
;;; empty queue is a cons cell where the cdr(content) is nil,
;;; and the car(last) is the cons itself.

;;; implementation
;;; A queue is a (last . contents) pair
(proclaim '(inline queue-contents make-queue enqueue deuque
            front empty-queue-p queue-nconc))

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q))
      (setf (car q) q))
  q)

(defun front (q)
  (first (queue-contents q)))

(defun empty-queue-p (q)
  (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))

;;; table
;;; trie
;;; user interface: make-trie, put-trie, get-trie, delete-trie.
(defstruct trie (value nil) (arcs nil))
(defconstant trie-deleted "deleted")

(defun put-trie (key trie value)
  "Set the value of key in trie."
  (setf (trie-value (find-trie key t trie)) value))

(defun get-trie (key trie)
  "Return the value for a key in a trie, and t/nil if found."
  (let* ((key-trie (find-trie key nil trie))
         (val (if key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eq val trie-deleted))
        (values nil nil)
        (values val t))))

(defun delete-trie (key trie)
  "Remove a key from a trie."
  (put-trie key trie trie-deleted))

(defun find-trie (key extend? trie)
  "Find the trie node for this key.
If EXTEND? is true, make a new node if need be."
  (cond ((null trie) nil)
        ((atom key)
         (follow-arc key extend? trie))
        (t (find-trie
            (cdr key) extend?
            (find-trie
             (car key) extend?
             (find-trie
              "." extend? trie))))))

(defun follow-arc (component extend? trie)
  "Find the trie node for this component of the key.
If EXTEND? is true, make a new node if need be."
  (let ((arc (assoc component (trie-arcs trie))))
    (cond ((not (null arc)) (cdr arc))
          ((not extend?) nil)
          (t (let ((new-trie (make-trie)))
               (push (cons component new-trie)
                     (trie-arcs trie))
               new-trie)))))

