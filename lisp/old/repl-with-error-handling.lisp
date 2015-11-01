(defmacro handling-errors (&body body) 
  `(HANDLER-CASE (progn ,@body) 
     (simple-condition 
         (ERR) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err))) 
       (apply (function format) *error-output* 
              (simple-condition-format-control   err) 
              (simple-condition-format-arguments err)) 
       (format *error-output* "~&") 
       (finish-output)) 
     (condition 
         (ERR) 
       (format *error-output* "~&~A: ~%  ~S~%" 
               (class-name (class-of err)) err) 
       (finish-output))))) 

(defun repl () 
  (do ((+eof+ (gensym)) 
       (hist 1 (1+ hist))) 
      (nil) 
    (format t "~%~A[~D]> " (package-name *package*) hist) 
    (finish-output) 
    (handling-errors 
     (setf - (read *standard-input* nil +eof+)) 
     (when (or (eq - +eof+) 
               (member - '((quit)(exit)(continue)) :test (function equal))) 
       (return-from repl)) 
     (let ((results (multiple-value-list (eval -)))) 
       (shiftf +++ ++ + -) 
       (shiftf /// // / results) 
       (shiftf *** ** * (first /))) 
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /) 
     (finish-output)))) 
