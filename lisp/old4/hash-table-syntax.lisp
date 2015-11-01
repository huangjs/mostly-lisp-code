(let ((mapping-types ()))
  (defun readtable-mapping-type (readtable)
    (check-type readtable readtable)
    (or (cdr (assoc readtable mapping-types)) :alist))

  (defun (setf readtable-mapping-type) (mode readtable)
    (check-type readtable readtable)
    (check-type mode (member :alist :plist :eq :eql :equal :equalp))
    (let ((existing (assoc readtable mapping-types)))
      (if existing
          (setf (cdr existing) mode)
        (push (cons readtable mode) mapping-types))))

  (defun mapping-reader (stream char)
    (declare (stream stream) (ignore char))
    (loop
        with mapping-type = (readtable-mapping-type *readtable*)
        with key and value
        with hashtable = (case mapping-type
                           (:eq     (make-hash-table :test #'eq))
                           (:eql    (make-hash-table :test #'eql))
                           (:equal  (make-hash-table :test #'equal))
                           (:equalp (make-hash-table :test #'equalp)))
        do
          (when (eql #\} (peek-char t stream t nil t))
            (read-char stream t nil t)
            (loop-finish))
          (setq key (read stream t nil t))
          (if (eql #\= (peek-char t stream t nil t))
              (read-char stream t nil t)
            (error 'parse-error :stream stream))
          (unless (eql #\> (read-char stream t nil t))
            (error 'parse-error :stream stream))
          (setq value (read stream t nil t))
          (case (peek-char t stream t nil t)
            (#\, (read-char stream t nil t))
            (#\} t)
            (t (error 'parse-error :stream stream)))
        if (eq mapping-type :alist) collect (cons key value) into list else
        if (eq mapping-type :plist) collect key into list
           and collect value into list else
        do (setf (gethash key hashtable) value)
        finally (case mapping-type
                  ((:alist :plist) (return (list 'quote list)))
                  (t (return hashtable))))))

;;   Now, suppose the above is available in the Common Lisp world.  A file
;;   that uses this syntax could go like this:

(eval-when (:execute :compile-toplevel)
  (setq *readtable* (copy-readtable))
  (set-macro-character #\{ #'mapping-reader)
  (set-syntax-from-char #\} #\)))

(eval-when (:execute :compile-toplevel)
  (setf (readtable-mapping-type *readtable*) :alist))

(defparameter *roman-alist*
    { "I" => 1,
      "V" => 5,
      "X" => 10,
      "L" => 50,
      "C" => 100,
      "D" => 500,
      "M" => 1000 })

(eval-when (:execute :compile-toplevel)
  (setf (readtable-mapping-type *readtable*) :plist))

(defparameter *roman-plist*
    { "I" => 1,
      "V" => 5,
      "X" => 10,
      "L" => 50,
      "C" => 100,
      "D" => 500,
      "M" => 1000 })

(eval-when (:execute :compile-toplevel)
  (setf (readtable-mapping-type *readtable*) :equal))

(defparameter *roman-hash*
    { "I" => 1,
      "V" => 5,
      "X" => 10,
      "L" => 50,
      "C" => 100,
      "D" => 500,
      "M" => 1000 }) 
