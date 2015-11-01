;;;# Introduction
;;; Here's what we want to write

;;; (string-case s
;;;   ("a" 0)
;;;   ("ab" 1)
;;;   ("abc" 2)
;;;   ("bc" 3)
;;;   (otherwise 4))

;;;# Naive Implementation

;;; A naive implementation would be to compare the string one by one.

(defmacro string-case (str &rest cases)
  (alexandria:once-only (str)
    (let ((clauses
            (loop for case in cases
                  for match = (car case)
                  for forms = (cdr case)
                  if (stringp match)
                  collect `((string= ,match ,str) ,@forms)
                  else if (eq match 'otherwise)
                  collect `(t ,@forms)
                  else
                  do (error "Case ~a is not recognized" case))))
      `(cond ,@clauses))))

;;; This will expand

#+nil
(string-case s
  ("a" 0)
  ("ab" 1)
  ("abc" 2)
  ("bc" 3)
  (otherwise 4))

;;; into

#+nil
(LET ((#:STR958 S))
  (COND ((STRING= "a" #:STR958) 0)
        ((STRING= "ab" #:STR958) 1)
        ((STRING= "abc" #:STR958) 2)
        ((STRING= "bc" #:STR958) 3)
        (T 4)))

;;; which is not optimal

;;;# Applying Partial Evaluation Techniques

