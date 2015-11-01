(in-package :shyogi)

(defun test-possible-moves ()
  (let ((positions (iter outer (for i below 5)
			 (iter (for j below 5)
			       (in outer
				   (collect (pos i j))))))
	(hu (make-unit :kind +hu+))
	(hisya (make-unit :kind +hisya+))
	(kaku (make-unit :kind +kaku+))
	(ginsyou (make-unit :kind +ginsyou+))
	(kinsyou (make-unit :kind +kinsyou+))
	(ou (make-unit :kind +ou+)))
    (mapcar (lambda (p) (hisya-moves hisya p)) positions)))
