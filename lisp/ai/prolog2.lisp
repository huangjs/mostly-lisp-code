;;; incremental prolog interpreter.
;;; automatic backtracking
;;; using depth first approach. need less memory.

;;; one approach is to use stream
;;; like SICP

;;; another approach
;;; like GPS
(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (some #'(lambda (clause)
            (let ((new-clause (rename-variables clause)))
              (prove-all
               (append (clause-body new-clause) other-goals)
               (unify goal (clause-head new-clause) bindings))))
        (get-clauses (predicate goal))))

;;; !!! incomplete
