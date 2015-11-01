;;; bit set -> list
(defun bset->list (bset universe)
  (declare (optimizable-series-function))
  (collect (choose (#Mlogbitp (scan-range :from 0) (series bset))
		   (scan universe))))

(defun list->bset (items universe)
  (declare (optimizable-series-function))
  (collect-fn 'integer #'(lambda () 0) #'logior
	      (mapping ((item (scan items)))
		       (ash 1 (bit-position item universe)))))

(defun bit-position (item universe)
  (declare (optimizable-series-function))
  (or (collect-first (positions (#Meq (series item) (scan universe))))
      (1- (length (nconc universe (list item))))))

(defun collect-logior (bsets)
  (declare (optimizable-series-function))
  (collect-fn 'integer #'(lambda () 0) #'logior bsets))

(defun collect-logand (bsets)
  (declare (optimizable-series-function))
  (collect-fn 'integer #'(lambda () -1) #'logand bsets))
