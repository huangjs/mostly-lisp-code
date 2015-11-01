(defun display-file (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (format t "~a%" line))
      (close in))))

(with-open-file (stream "c:/temp.txt")
  (format t "~a~%" (read-line stream)))

(with-open-file (stream "c:/temp.txt" :direction :output)
  (format stream "Some text."))



