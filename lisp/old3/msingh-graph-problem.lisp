(defparameter *graph*
  '((3 (7 11 17 31 37 59 67 73))
    (7 (19 61 97))
    (11 (23))
    (13 (19 61))
    (17 (83))
    (19 (31 79 97))
    (23 (47 89))
    (29 (71))
    (37 (67 79))
    (43 (97))))

(defun range (&rest args)
  (ecase (length args)
    (1 (iter (for i from 1 to (first args)) (collect i)))
    (2 (iter (with (start end) = args)
	     (for i from start to end)
	     (collect i)))
    (3 (iter (with (start end step) = args)
	     (for i from start to end by step)
	     (collect i)))))

(defun primep (num)
  (cond ((evenp num) (when (= num 2) num))
	((> num 7)
	 (when (iter (for i from 3 to (isqrt num) by 2)
		     (never (zerop (mod num i))))
	   num))
	(t (find num '(3 5 7)))))

(org.tfeb.hax.memoize:memoize-function 'primep)

(defun pairwise-compatible-primes (p1 p2)
  (flet ((number-conc (n1 n2)
	   (parse-integer (format nil "~D~D" n1 n2))))
    (and (primep (number-conc p1 p2))
	 (primep (number-conc p2 p1)))))



(defun branches (node graph)
  (second (find node graph :key #'first)))

(defun direct-connected-p (node1 node2 graph)
  (or (member node1 (branches node2 graph))
      (member node2 (branches node1 graph))))

(defun outer (combinator &rest lists)
  (cond ((null lists)
	 nil)
	((null (cdr lists))
	 (car lists))
	(t
	 (iter (with (1st 2nd . rst) = lists)
	       (return
		 (apply #'outer
			combinator
			(iter outer-col (for e1 in 1st)
			      (iter (for e2 in 2nd)
				    (in outer-col
					(collect (funcall combinator e1 e2)))))
			rst))))))

(defun paths-start-from (node graph)
  (iter (with branches = (branches node graph))
	(with paths = `((,node)))
	(when (null branches)
	  (return paths))
	(for n in branches)
	(nconcing
	 (outer #'append paths (paths-start-from n graph)))))


(defun build-prime-graph (primes)
  (iter (for (p . rst) on primes)
	(collect
	 (list p (iter (for q in rst)
		       (when (pairwise-compatible-primes p q)
			 (collect q)))))))


(defun solv (n end)
  "return all the lists at-least of length n, in the paths of pairwise-compatible-primes graph"
  (let* ((primes (delete nil (mapcar #'primep (range end))))
	 (graph (build-prime-graph primes)))
    (remove-if-not (lambda (l) (>= (length l) n))
		   (mapcan (lambda (n) (paths-start-from n graph)) primes))))


#|
example

(solv 5 110) => ((3 11 23 89 107))
(first (sort (solv 10 1000) #'> :key #'length)) => (3 7 19 97 157 181 199 211 283 397 547 643 751 787 823 877 883 991)

(values-list (mapcar (lambda (n) (paths-start-from n *graph*))
		     (mapcar #'first *graph*)))

=>

((3 7 19 31) (3 7 19 79) (3 7 19 97) (3 7 61) (3 7 97) (3 11 23 47)
 (3 11 23 89) (3 17 83) (3 31) (3 37 67) (3 37 79) (3 59) (3 67) (3 73))
((7 19 31) (7 19 79) (7 19 97) (7 61) (7 97))
((11 23 47) (11 23 89))
((13 19 31) (13 19 79) (13 19 97) (13 61))
((17 83))
((19 31) (19 79) (19 97))
((23 47) (23 89))
((29 71))
((37 67) (37 79))
((43 97))

|#
