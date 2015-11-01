;; (declaim (optimize (speed 0) debug safety))

(defparameter *tsv-delimiter* #\Tab)

(defun string-trim-whitespace (string)
  (string-trim '(#\Space #\Tab #\　) string))

(defun split-tsv-line (line)
  (split-sequence:split-sequence
   *tsv-delimiter*
   line))

(defun read-tsv-line (stream)
  (let ((line (string-trim-whitespace (read-line stream nil ""))))
    (when (and line (not (zerop (length line))))
      (split-tsv-line line))))

(defun count-columns (line)
  (let* ((len1 (length line))
         (last-column (first (last line)))
         (colon-pos (position #\: last-column))
         (maybe-length (when colon-pos
                         (1+ (parse-integer (subseq last-column 0 colon-pos))))))
    (max len1 (or maybe-length 0))))

(defun parse-cell (cell-string type)
  (let* ((colon-position (position #\: cell-string))
         (data (subseq cell-string (1+ (or colon-position -1))))) 
    (ecase type
      (double-float
         (float (read-from-string data) 0d0))
      (symbol
         (let ((result (read-from-string data)))
           (assert (symbolp result))
           result))
      (string
         data))))

(defun read-tsv-stream (stream &key types)
  (let* ((first-line (read-tsv-line stream))
         (ncol (count-columns first-line))
         (types (cond ((null types)
                       (make-list ncol :initial-element 'double-float))
                      ((symbolp types)
                       (make-list ncol :initial-element types))
                      (t types))))
    (loop for line = first-line then (read-tsv-line stream)
          for count from 1
          while line
          do (when (zerop (rem count 1000))
               (print count)
               (finish-output))
          collect
       (if (= (length line) ncol)
           (loop for cell in line
                 for type in types
                 collect
              (parse-cell cell type))
           (warn  "In line: ~a, ncols is ~a, should be ~a" count (length line) ncol)))))


