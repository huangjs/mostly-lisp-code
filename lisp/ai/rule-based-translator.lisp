;;; (load "essential")
;;; (load "pattern-matching-tools")

(defun rule-based-translator (input
                              rules
                         &key (matcher #'pat-match)
                              (rule-if #'first)
                              (rule-then #'rest)
                              (action #'sublis))
  "Find the first rule in rules that matches input,
and apply the action to that rule."
  (some
   #'(lambda (rule)
       (let ((result (funcall matcher (funcall rule-if rule)
                              input)))
         (if (not (eq result fail))
             (funcall action result (funcall rule-then rule)))))
   rules))

;;; example
(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (rule-based-translator input *eliza-rules*
                         :action #'(lambda (bindings responses)
                                     (sublis (switch-viewpoint bindings)
                                             (random-elt responses)))))
