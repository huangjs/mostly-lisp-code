(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf :infix))

(defun sum (seq)
  (reduce #'+ seq))

(defun product (seq)
  (reduce #'* seq))

(defun anytrue (seq)
  (reduce #'(lambda (a b) (or a b)) seq))

(defun alltrue (seq)
  (reduce #'(lambda (a b) (and a b)) seq))

(defun myappend (list-a list-b)
  (reduce #'cons list-a :initial-value list-b :from-end t))

(defun mymap (fn list)
  (reduce #'(lambda (a b) (cons (funcall fn a) b))  list :initial-value nil :from-end t))

(defun summatrix (matrix-by-sequence)
  (sum (map 'vector #'sum matrix-by-sequence)))

(defun make-tree (node &rest subtrees)
  (cons node subtrees))

(defun node (tree)
  (car tree))

(defun subtrees (tree)
  (cdr tree))

(defun nodep (tree)
  "Return true if the tree only contains a node."
  (null (subtrees tree)))

(defun rec-apply (fn arglist)
  (if (null (cdr arglist))
	  (car arglist)
	  (funcall fn (car arglist) (rec-apply fn (cdr arglist)))))

;;; does'nt work!
;; (defun redtree (node-fn subtree-fn init-value tree)
;;   (labels ((iter (tree)
;; 				 (let ((node (node tree))
;; 					   (subtrees (subtrees tree)))
;; 				   (if (null subtrees)
;; 					   node
;; 					   (funcall node-fn node
;; 								(rec-apply subtree-fn
;; 										   (mapcar #'iter subtrees)))))))
;; 	(funcall node-fn (iter tree) init-value)))

(defun redtree (node-fn subtree-fn init-value tree)
  (labels ((iter (tree)
				 (funcall node-fn (node tree) (redsubtrees (subtrees tree))))
		   (redsubtrees (trees)
			 (if (null trees)
				 init-value
				 (funcall subtree-fn (iter (first trees))
						  (redsubtrees (rest trees))))))
	(iter tree)))

;;; example
;; (setf x (make-tree 1 (make-tree 2) (make-tree 3 (make-tree 4))))

(defun flat-tree (tree)
  (redtree #'cons #'append nil tree))

(defun map-tree (fn tree)
  (redtree #'(lambda (node subtrees)
			   (apply #'make-tree (funcall fn node) subtrees))
		   #'cons
		   nil
		   tree))

;;; stream
(defun next (N x)
  (with-type double-float
	#i "(x + N/x)/2d0"))

(defun repeat (f a)
  (declare (double-float a))
  (ll:cons a (repeat f (funcall f a))))

(defun within (eps inf-stream)
  (declare (double-float eps))
  (let ((a (ll:car inf-stream))
		(b (ll:car (ll:cdr inf-stream)))
		(rest (ll:cdr (ll:cdr inf-stream))))
	(if (<= (abs (- a b)) eps)
		b
		(within eps (ll:cons b rest)))))

(defun relative (eps inf-stream)
  (declare (double-float eps))
  (let ((a (ll:car inf-stream))
		(b (ll:car (ll:cdr inf-stream)))
		(rest (ll:cdr (ll:cdr inf-stream))))
	(if (<= (abs (- a b)) (* eps (abs b)))
		b
		(relative eps (ll:cons b rest)))))

(defun mysqrt (a0 eps n)
  (within eps (repeat (curry #'next n) a0)))

(defun mysqrt-relative (a0 eps n)
  (relative eps (repeat (curry #'next n) a0)))

(defun easydiff (f x h)
  (declare (double-float x h))
  (macrolet ((f (x)
			   `(funcall f ,x)))
	#i "(f(x+h)-f(x))/h"))

(defun halve (x)
  (declare (double-float x))
  (/ x 2))

(defun differentiate (h0 f x)
  (ll:mapcar #'(lambda (h)
				 (easydiff f x h))
			 (repeat #'halve h0)))

;;; FIXME: improve is buggy
(defun elimerror (n inf-stream)
  (declare (double-float n))
  (let ((a (ll:car inf-stream))
		(b (ll:car (ll:cdr inf-stream))))
	(ll:cons #i"(b*(2^^n)-a)/(2^^n-1)"
			 (elimerror n (cons b (ll:cdr inf-stream))))))

(defun order (inf-stream)
  (let* ((a (ll:car inf-stream))
		 (r1 (ll:cdr inf-stream))
		 (b (ll:car r1))
		 (r2 (ll:cdr r1))
		 (c (ll:car r2)))
	(coerce (round (log #i"(a-c)/(b-c) -1" 2)) 'double-float)))

(defun improve (stream)
  (elimerror (order stream) stream))

(defun easyintegrate (f a b)
  (declare (double-float a b))
  (/ (* (+ (funcall f a)
		   (funcall f b))
		(- b a))
	 2))

(defun zip (xs ys)
  (if (or (null xs) (null ys))
	  nil
	  (ll:cons (cons (ll:car xs) (ll:car ys))
			   (zip (ll:cdr xs) (ll:cdr ys)))))

(defun addpair (pair)
  (+ (car pair) (cdr pair)))

(defun integrate (f a b)
  (declare (double-float a b))
  (let ((mid (/ (+ a b) 2)))
	(ll:cons (easyintegrate f a b)
			 (ll:mapcar #'addpair (zip (integrate f a mid)
									   (integrate f mid b))))))

