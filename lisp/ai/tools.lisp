;;; an interactive interpreter tool
(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
      (print prompt)
      (print (funcall transformer (read)))))

;;; example interpreters
(defun lisp ()
  (interactive-interpreter '> #'eval))

(defun eliza ()
  (interactive-interpreter 'eliza>
      #'(lambda (x)
          (flatten (use-eliza-rules x)))))

;;; or
(defun eliza ()
  (interactive-interpreter 'eliza>
     (compose #'flatten #'use-eliza-rules)))

;;; add two features
;;; 1. handle errors
;;; 2. allows the prompt to be either a string or a function
(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
      (handler-case
          (progn
            (if (stringp prompt)
                (print prompt)
                (funcall prompt))
            (print (funcall transformer (read))))
        ;; In case of error, do this:
        (error (condition)
               (format t "~&;; Error ~a ignored, back to top level."
                       condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [1], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))
