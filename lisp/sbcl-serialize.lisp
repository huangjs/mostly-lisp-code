(defun make-output (filename)
  (let* ((stream (open filename
                       :direction :output
                       :if-exists :supersede
                       :element-type 'sb-assem:assembly-unit))
         (res (make-fasl-output :stream stream)))
    res))

(defun serialize (object output)
  (sb-fasl:dump-object object output))

