(eval-when (compile)
  (setf (get 'golden-section-combination 'sys::immed-args-call)
	'((double-float double-float) double-float))
  )

(defun golden-section-combination (a b)
  "Return the convex combination (1-G)*a+G*b, where G is the
inverse of the golden ratio."
  (declare (optimize speed (safety 0))
           (double-float a b))
  (let ((Gright #.(/ (- 3d0 (sqrt 5d0)) 2d0)) ; equals to G above
        (Gleft #.(- 1d0 (/ (- 3d0 (sqrt 5d0)) 2d0)))) ; 1-G
    (+ (* Gleft a) (* Gright b)))) 
