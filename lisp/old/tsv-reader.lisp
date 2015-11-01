(let ((*read-eval* nil)) 
  (with-open-file (file "x^2.txt") 
    (loop for line = (read-line file nil nil) 
	   while line 
	   collect (ignore-errors (read-from-string (format nil "(~A)" line))))))

;;; or

(let ((*read-eval* nil)) 
  (with-open-file (file "x^2.txt") 
	(loop for left  = (read file nil nil) 
	   for right = (read file nil nil) 
	   while right					   ; or perhaps: (and left right) 
	   collect (list left right)))) 

;;; or

(asdf :split-sequence)
(asdf :parse-number)

(with-open-file (s "programming/lisp/x^2.txt") 
  (loop for line = (read-line s nil nil) 
	 while line 
	 collect (mapcar (lambda (e)
					   (org.mapcar.parse-number:parse-number e))
					 (split-sequence:split-sequence #\space line :remove-empty-subseqs t)))) 

