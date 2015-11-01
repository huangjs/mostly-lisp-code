(defun ex1 ()
  (let ((x (.rseq 0d0 (* 2d0 pi) 100)))
    (plot (.sin x) x
	  :title "Sine Curve"
	  :xlabel "Theta"
	  :ylabel "Sin(theta)")))
