;;; same shape predicate
(defun same-shape-tree-p (a b)
  "Are two trees the same except for the leaves?"
  (tree-equal a b :test #'true))

(defun true (&rest ignore)
  "Always return true."
  t)

(defmacro dotree ((name tree &optional ret-val) &body body)
  "Evaluate BODY with NAME bound to every element in TREE. Return RET-VAL."
  (with-unique-names (traverser list list-element)
    `(progn
       (labels ((,traverser (,list)
                  (dolist (,list-element ,list)
                    (if (consp ,list-element)
                        (,traverser ,list-element)
                        (let ((,name ,list-element))
                          ,@body)))))
         (,traverser ,tree)
         ,ret-val))))

(defun tree-paths (tree)
  "Get all the paths that reach to the terminal nodes.
e.g. (tree-paths '(77 (237 (228 (234 (231 266) 266)) (234 (231 266) 266))) )
     => 
     ((77 237 228 234 231 266) (77 237 228 234 266) (77 237 234 231 266)
 (77 237 234 266))"
  (if (atom tree) 
	  `((,tree)) 
	  (destructuring-bind (root &rest nodes) tree 
		(mapcar #'(lambda (sub-path) 
					`(,root ,@sub-path)) 
				(mapcan #'tree-paths nodes)))))


