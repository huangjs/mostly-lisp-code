(deftype index ()
  '(signed-byte 28))

(declaim (type store *dna*))
(defvar *dna* nil)
(declaim (type index *dna-index*))
(defvar *dna-index* nil)
(defvar *rna* nil)

(declaim (optimize speed (safety 0))) ;; (debug 2)))

(defstruct store
  (chunks nil :type list)
  (cache-start nil)
  (cache-chunk nil)
  (length 0 :type index))

(defmacro sv (data)
  `(coerce ,data 'simple-base-string))

(defstruct chunk
  (length 0 :type index)
  (offset 0 :type index)
  (sv (sv "") :type simple-base-string))

(defun make-store-from-strings (strings)
  (let ((length 0))
    (declare (type index length))
    (setf strings (remove 0 strings :key #'length))
    (make-store :chunks (mapcar (lambda (sv)
                                  (declare (type (and simple-array vector)
                                                 sv))
                                  (incf length (length sv))
                                  (make-chunk :sv (sv sv)
                                              :length (length sv)
                                              :offset 0))
                                strings)
                :length length)))

(defparameter *dna-string*
  (with-open-file (stream (merge-pathnames #p"endo.dna"
                                           (load-time-value *load-pathname*)))
    (read-line stream)))

(defun execute (prefix &optional suffix)
  (let ((*dna* (with-open-file (stream (merge-pathnames #p"endo.dna"
                                                        (load-time-value *load-pathname*)))
                 (make-store-from-strings (list prefix
                                                (or suffix
                                                    (read-line stream))))))
        (*print-length* 100)
        (*print-circle* t)
        (*print-pretty* nil)
        (*dna-index* 0))
    (unwind-protect
         (progn
           (setf (bytes-consed-between-gcs) (* 128 1024 1024))
           (setf *rna* nil)
           (catch 'finish
             (loop for index fixnum from 0
                   for last-rna = nil
                   for pattern = (reverse (pattern))
                   for template = (reverse (template))
                   do (when (zerop (mod index 10000))
                        (format t "iteration: ~a~%" index))
                   do (match-replace pattern template)
                   do (condense-dna))))
      (setf (bytes-consed-between-gcs) (* 12 1024 1024)))))

(defun rna (rna)
  (push rna *rna*))

(defun finish ()
  (throw 'finish nil))

(defun chunk-ref (chunk index)
  (declare (optimize speed)
           (type index index))
  (aref (chunk-sv chunk)
         (+ (the index (chunk-offset chunk))
            index)))

(defun condense-dna ()
  (when (> (length (store-chunks *dna*)) 250)
    (let ((sv (make-array (store-length *dna*) :element-type 'base-char)))
      (loop for c in (store-chunks *dna*)
            for start = 0 then end
            for end of-type index = (chunk-length c) then (+ (chunk-length c) end)
            do (replace sv (chunk-sv c)
                        :start1 start :end1 end
                        :start2 (chunk-offset c) :end2 (+ (chunk-offset c)
                                                          (chunk-length c))))
      (setf (store-chunks *dna*)
            (list (make-chunk :sv sv
                              :length (length sv)
                              :offset 0))))))

(defun store-ref (store index)
  (declare (type index index))
  (let ((res (loop with s = 0
                   for chunks on (if (and (store-cache-start store)
                                          (<= (the fixnum
                                                (store-cache-start store))
                                              index))
                                     (progn
                                       (setf s (store-cache-start store))
                                       (store-cache-chunk store))
                                     (store-chunks store))
                   for chunk = (car chunks)
                   for start of-type index = s then end
                   for end of-type index = (+ start (chunk-length chunk))
                   do (setf (store-cache-start store) start
                            (store-cache-chunk store) chunks)
                   when (and (>= index start)
                             (< index end))
                   return (chunk-ref chunk
                                     (- index start)))))
    res))

(defun dref (n)
  (declare (type index n))
  (if (>= n (store-length *dna*))
      (finish)
      (store-ref *dna* (+ (the index *dna-index*)
                          n))))

(defun store-subseq (store start &optional (end (store-length store)))
  (sv (loop for i of-type index from start below end
            collect (store-ref store i))))

(defun store-subseq/chunk (store start &optional (end (store-length store)
                                                      endp))
  (declare (optimize speed)
           (type sb-impl::index start end))
  (unless (< start end)
    (return-from store-subseq/chunk
      (list (load-time-value (make-chunk :sv (sv "") :length 0)))))
  (let* ((first-offset nil)
         (overrun -1)
         (chunks (if endp
                     (loop for x in (store-chunks store)
                           for start* of-type sb-impl::index = 0 then end*
                           for end* of-type sb-impl::index = (chunk-length x) then (+ end* (chunk-length x))
                           while (< overrun 0)
                           when (or (and (<= start* start)
                                         (> end* start))
                                    first-offset)
                           collect (progn
                                     (unless first-offset
                                       (setf first-offset (- start start*)))
                                     (setf overrun (- end* end))
                                     x))
                     (loop for chunks on (store-chunks store)
                           for x = (car chunks)
                           for start* of-type sb-impl::index = 0 then end*
                           for end* of-type sb-impl::index = (chunk-length x) then (+ end* (chunk-length x))
                           when (or (and (<= start* start)
                                         (> end* start))
                                    first-offset)
                           do (progn
                                (setf first-offset (- start start*))
                                (return chunks))))))
    (let ((first (car chunks)))
      (setf (car chunks)
            (copy-chunk-with-offset first first-offset))
      (when (> overrun 0)
        (let ((lastcons (last chunks)))
          (let ((copy (copy-chunk (car lastcons))))
            (decf (chunk-length copy) overrun)
            (setf (car lastcons) copy)))))
    chunks))

;; Test image
;; (execute "IIPIFFCPICICIICPIICIPPPICIIC") (build)
;; Guide 1
;; (execute "IIPIFFCPICFPPICIICCIICIPPPFIIC") (build)
;; Guide 2
;; (execute "IIPIFFCPICFPPICIICCCIICIPPPCFIIC") (build)
;; Guide 3
;; (execute "IIPIFFCPICFPPICIICCCIICIPPPFFIIC") (build)
;; Catalog (1337)
;; (execute "IIPIFFCPICFPPICIICCCCCCCCCCCCIICIPPPFCCFFFCCFCFIIC") (build)
;; Genomics (1729)
;; (execute "IIPIFFCPICFPPICIICCCCCCCCCCCCIICIPPPFCCCCCFFCFFIIC") (build)

(defun copy-chunk-with-offset (chunk offset)
  (declare (type index offset))
  (let ((copy (copy-chunk chunk)))
    (incf (chunk-offset copy) offset)
    (decf (chunk-length copy) offset)
    copy))

(defun pattern ()
  (let ((level 0)
        (pattern nil))
    (declare (type (integer 0 32000) level))
    (loop for head = (dref 0)
          do (ecase head
               ((#\C)
                (incf *dna-index*)
                (push #\I pattern))
               ((#\F)
                (incf *dna-index*)
                (push #\C pattern))
               ((#\P)
                (incf *dna-index*)
                (push #\F pattern))
               ((#\I)
                (let ((mid (dref 1)))
                  (ecase mid
                    ((#\C)
                     (incf *dna-index* 2)
                     (push #\P pattern))
                    ((#\P)
                     (incf *dna-index* 2)
                     (push (nat) pattern))
                    ((#\F)
                     (incf *dna-index* 3)
                     (push (sv (the list (consts)))
                           pattern))
                    ((#\I)
                     (let ((tail (dref 2)))
                       (ecase tail
                         ((#\P)
                          (incf *dna-index* 3)
                          (incf level)
                          (push :open pattern))
                         ((#\C #\F)
                          (incf *dna-index* 3)
                          (when (zerop level)
                            (return-from pattern pattern))
                          (decf level)
                          (push :close pattern))
                         ((#\I)
                          (rna (store-subseq *dna*
                                             (+ *dna-index* 3)
                                             (+ *dna-index* 10)))
                          (incf *dna-index* 10))))))))))))

(defun nat ()
  (loop for head = (dref 0)
        with sum fixnum = 0
        for mask fixnum = 1 then (ash mask 1)
        do (incf *dna-index*)
        do (case head
             ((#\P)
              (return sum))
             ((#\C)
              (setf sum (logior mask sum))))))

(defun consts ()
  (loop for head = (dref 0)
        collect (case head
                  ((#\C) #\I)
                  ((#\F) #\C)
                  ((#\P) #\F)
                  ((#\I)
                   (let ((tail (dref 1)))
                     (if (eql tail #\C)
                         (progn
                           (incf *dna-index*)
                           #\P)
                         (loop-finish)))))
        do (incf *dna-index*)))

(defun template ()
  (let ((template nil))
    (loop for head = (dref 0)
          do (ecase head
               ((#\C)
                (incf *dna-index*)
                (push #\I template))
               ((#\F)
                (incf *dna-index*)
                (push #\C template))
               ((#\P)
                (incf *dna-index*)
                (push #\F template))
               ((#\I)
                (let ((mid (dref 1)))
                  (ecase mid
                    ((#\C)
                     (incf *dna-index* 2)
                     (push #\P template))
                    ((#\F #\P)
                     (incf *dna-index* 2)
                     (push (cons (nat) (nat))
                           template))
                    ((#\I)
                     (let ((tail (dref 2)))
                       (ecase tail
                         ((#\P)
                          (incf *dna-index* 3)
                          (push (nat) template))
                         ((#\C #\F)
                          (incf *dna-index* 3)
                          (return-from template template))
                         ((#\I)
                          (rna (store-subseq *dna*
                                             (+ *dna-index* 3)
                                             (+ *dna-index* 10)))
                          (incf *dna-index* 10))))))))))))

(defun match-replace (pattern template)
  (declare (optimize speed (safety 0)))
  (let ((i *dna-index*)
        (env nil)
        (c nil))
    (declare (type sb-impl::index i))
    (dolist (p pattern)
      (typecase p
        (character
         (if (eql (store-ref *dna* i) p)
             (incf i)
             (progn
               (return-from match-replace))))
        (fixnum
         (incf i p)
         (when (> i (store-length *dna*))
           (return-from match-replace)))
        (simple-base-string
         (loop for j of-type sb-impl::index from i
               for k of-type (signed-byte 60) from 0 below (length p)
               do (when (>= j (store-length *dna*))
                    (return-from match-replace))
               do (when (not (eql (store-ref *dna* j)
                                  (the character (aref p k))))
                    (decf j k)
                    (setf k -1))
               finally (setf i j)))
        ((member :open)
         (push i c))
        ((member :close)
         (let* ((j (pop c))
                (chunk (store-subseq/chunk *dna* j i)))
           (push chunk env)))))
    (let* ((replacement (replace-template template (reverse env)))
           (suffix (store-subseq/chunk *dna* i))
           (chunks (append (reverse replacement) suffix)))
      (setf *dna* (make-store :chunks chunks
                              :length (the fixnum
                                        (+ (loop for c in replacement
                                                 for i fixnum = (chunk-length c) then (+ i (chunk-length c))
                                                 finally (return i))
                                           (store-length *dna*)
                                           (- i))))
            *dna-index* 0))))

(defun replace-template (template env)
  (let ((collect nil)
        (chars nil))
    (flet ((collect (x)
             (when chars
               (let* ((len (length (the list chars)))
                      (array (make-array len :element-type 'base-char)))
                 (dotimes (i len)
                   (setf (aref array (- len i 1)) (pop chars)))
                 (push (make-chunk :sv array
                                   :length len)
                       collect)))
             (when x
               (push x collect))))
      (dolist (fragment template)
        (etypecase fragment
          (character
           (push fragment chars))
          (fixnum
           (if (nth fragment env)
               (dolist (c (asnat (loop for c in (nth fragment env)
                                       for i fixnum = (chunk-length c) then (+ i (chunk-length c))
                                       finally (return i))))
                 (push c chars))
               (if chars
                   (push #\P chars)
                   (collect (load-time-value (make-chunk :sv (sv "P")
                                                         :length 1))))))
          (cons
           (let ((level (car fragment))
                 (group (or (nth (cdr fragment) env)
                            (load-time-value
                             (list (make-chunk :sv (sv "")
                                               :length 0))))))
             (declare (type index level))
             (dolist (group group)
               (if (zerop level)
                   (if (< (chunk-length group) 128)
                       (dotimes (i (chunk-length group))
                         (push (chunk-ref group i) chars))
                       (collect group))
                   (collect (protect level group))))))))
      (collect nil))
    collect))

(defun asnat (n)
  (declare (type fixnum n))
  (loop with finish = nil
        until finish
        collect (cond ((zerop n)
                       (setf finish t)
                       #\P)
                      (t
                       (let ((oddp (oddp n)))
                         (setf n (ash n -1))
                         (if oddp
                             #\C
                             #\I))))))

(defun protect (level dna)
  (dotimes (i (the fixnum level))
    (setf dna (quote-dna dna)))
  dna)

(defun quote-dna (chunk)
  (let ((sv (sv (the list (loop for i from 0 below (chunk-length chunk)
                                for base = (chunk-ref chunk i)
                                collect (case base
                                          (#\I #\C)
                                          (#\C #\F)
                                          (#\F #\P)
                                          (#\P #\I))
                                when (eql base #\P) collect #\C)))))
    (make-chunk :sv sv
                :length (length sv)
                :offset 0)))

(defun quote-dna-string (string)
  (sv (the list (loop for base across (the simple-base-string string)
                      collect (case base
                                (#\I #\C)
                                (#\C #\F)
                                (#\F #\P)
                                (#\P #\I))
                      when (eql base #\P) collect #\C))))
