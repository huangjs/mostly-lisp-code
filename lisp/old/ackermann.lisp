(defun ackermann (m n)
  "The ackermann function, make sure that m and n are positive!"
  (when (and (>= m 0) (>= n 0))
    (cond ((= m 0)
           (+ n 1))
          ((= n 0)
           (ackermann (- m 1) 1))
          (t (ackermann (- m 1)
                        (ackermann m (- n 1)))))))

