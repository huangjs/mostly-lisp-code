;;; (load "essential")

;;; structure comparison
(defun same-shape-tree? (a b)
  "Are two trees the same except for the leaves (values)?"
  (tree-equal a b :test #'true))

