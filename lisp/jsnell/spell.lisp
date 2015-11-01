(require :sb-sprof)

(defvar *words* (make-hash-table :test 'equalp))

(defvar *alphabets*
  (loop for i from (char-code #\A) to (char-code #\Z)
        collect (string (code-char i))))

(loop repeat 100000
      for word = (format nil "~36r" (random (expt 2 16)))
      do (setf (gethash word *words*) 1))

(defun conc (&rest args)
  (declare (optimize speed))
  (let* ((length (reduce #'+ args :key #'length))
         (res (make-string length)))
    (loop for string in args
          for start = 0 then end
          for end = (+ start (length string))
          do (replace res
                      (the (simple-array character)
                        string)
                      :start1 start :end1 end))
    res))

(defun edits1 (word)
  (let ((length (length word))
        (words (make-hash-table :test 'equalp)))
    (declare (type (simple-array character) word)
             (optimize speed))
    ;; delete
    (loop for i from 0 below length
          do (setf (gethash (conc (subseq word 0 i) (subseq word (1+ i)))
                            words)
                   t))
    ;; insertion
    (loop for i from 0 to length
          do (dolist (c *alphabets*)
               (setf (gethash (conc (subseq word 0 i)
                                    c
                                    (subseq word i))
                              words)
                     t)))
    ;; transposition
    (loop for i from 0 below (1- length) collect
          (setf (gethash (conc (subseq word 0 i)
                               (string (char word (1+ i)))
                               (string (char word i))
                               (subseq word (+ 2 i)))
                         words)
                t))
    ;; alteration
    (loop for i from 0 below length
          do (dolist (c *alphabets*)
               (setf (gethash (conc (subseq word 0 i) c
                                    (subseq word (1+ i)))
                              words)
                     t)))
    (loop for key being the hash-keys of words
          collect key)))

(defun correct (word)
  (let ((foo nil))
    (dolist (edit1 (edits1 word) foo)
      (dolist (edit2 (edits1 edit1))
        (when (gethash edit2 *words*)
          (push edit2 foo))))))

(sb-sprof:with-profiling (:max-samples 50000 :report :flat :show-progress t)
  (time
   (loop repeat 10
         do (correct (format nil "~36r" (random (expt 2 16)))))))
