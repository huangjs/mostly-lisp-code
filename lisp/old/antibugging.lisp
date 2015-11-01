;;; use antibugging tools as a system level test.

;;; cerror - continuable error.
(defun average (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
      (/ (reduce #'+ number)
         (length numbers))))

;;; check type
(defun sqr (x)
  "Multiply x by itself."
  (check-type x number)
  (* x x))

;;; or use assert -- more general
(defun sqr (x)
  "Multiply x by itself."
  (assert (numberp x))
  (* x x))

;;; the previous assert cannot supply new values,not good enough
(defun sqr (x)
  "Multiply x by itself."
  (assert (numberp x) (x))
  (* x x))

;;; a complicated one
;;; example
(defun eat-porridge (bear)
  (assert (< too-cold (temperature (bear-porridge bear)) too-hot)
          (bear (bear-porridge bear))
          "~a's porridge is not just right: ~a"
          bear (hotness (bear-porridge bear)))
  (eat (bear-porridge bear)))
