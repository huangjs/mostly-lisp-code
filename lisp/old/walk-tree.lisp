(defun dft (tree) 
  (cond ((null tree) nil) 
		((atom tree) (princ tree)) 
		(t (dft (car tree)) 
		   (dft (cdr tree))))) 

;;; lexical binding
(setf *saved* nil) 

(=defun dft-node (tree) 
  (cond ((null tree) (restart)) 
		((atom tree) (=values tree)) 
		(t (push #'(lambda () (dft-node (cdr tree))) 
				   *saved*) 
		   (dft-node (car tree))))) 

(=defun restart () 
  (if *saved* 
	  (funcall (pop *saved*)) 
	  (=values 'done))) 

(=defun dft2 (tree) 
  (setq *saved* nil) 
  (=bind (node) (dft-node tree) 
	(cond ((eq node 'done) (=values nil)) 
		  (t (princ node) 
			 (restart))))) 

;;; example
(setf t1 '(a (b (d h)) (c e (f i) g)) 
	  t2 '(1 (2 (3 6 7) 4 5)))

(=bind (node1) (dft-node t1) 
  (if (eq node1 'done) 
	  'done 
	  (=bind (node2) (dft-node t2) 
		(list node1 node2)))) 




