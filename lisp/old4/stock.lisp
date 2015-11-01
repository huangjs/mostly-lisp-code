(defun stop-loss-price (original-price &optional (stop-loss-percent 0.088))
  (* original-price (- 1 stop-loss-percent)))
