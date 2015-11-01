(defun remaining-safak ()
  "Return how many days left."
  (- (encode-universal-time 0 0 0 16 9 2008)
     (get-universal-time)))

(defparameter *time-bases*       '(60       60        24    30     12))
(defparameter *time-bases-names* '("second" "minute" "hour" "day" "month" "year"))

(defmacro define-base-decomposer (name bases)
  "Note: BASES is evaluated."
  `(defun ,name (value)
     (labels ((decompose (value bases result)
                (if (null bases)
                    (reverse (cons (unless (zerop value) value) result))
                    (multiple-value-bind (q r) (truncate value (car bases))
                      (decompose q (cdr bases)
                                 (cons (unless (zerop r) r) result))))))
       (decompose value ,bases '()))))

(define-base-decomposer time-interval-to-components *time-bases*)

(defun format-decomposed-value (decomposed-values bases)
  (substitute #\. #\,
	      (format nil "~:{~:[~*~;~:*~A ~A~2:*~P~*, ~]~}"
		      (mapcar (function list)
			      (reverse decomposed-values)
			      (reverse bases)))
	      :from-end t :count 1))

;; (format-decomposed-value (time-interval-to-components (remaining-safak))
;; 			 *time-bases-names*)
