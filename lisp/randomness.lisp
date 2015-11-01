;;; only in SBCL

(defun get-truly-random-bytes (nbytes &optional result)
  (with-open-file (f "/dev/urandom" :element-type '(unsigned-byte 32))
    (let ((result (or result (make-array nbytes :element-type '(unsigned-byte 32)))))
      (read-sequence result f)
      result)))

(setf *random-state*
      (seed-random-state (get-truly-random-bytes 16)))

