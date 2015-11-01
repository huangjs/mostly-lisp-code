;;;; Utilities to mmap a file directly into an SBCL base string

(defvar +page-size+ (sb-posix:getpagesize))

(defmacro with-mmaped-base-string ((string file) &body body)
  (let ((handle-var (gensym))
        (stream-var (gensym)))
    `(with-open-file (,stream-var ,file)
       (let ((,handle-var (mmap-as-base-string ,stream-var)))
         (unwind-protect
              (let ((,string (mmap-handle-string ,handle-var)))
                ,@body)
           (mmap-close ,handle-var))))))

(defstruct mmap-handle
  (string (coerce "" 'base-string) :type simple-base-string)
  fd
  address
  length)

(defun mmap-close (handle)
  (sb-posix:munmap (mmap-handle-address handle)
                   (mmap-handle-length handle)))

(defun mmap-as-base-string (stream)
  (declare (optimize debug)
           (notinline sb-posix::mmap))
  (with-open-file (devnull "/dev/null")
    (let* ((file-length (file-length stream))
           (map-length (+ file-length +page-size+))
           (sap1 (sb-posix:mmap nil
                                map-length
                                (logior sb-posix:prot-read sb-posix:prot-write)
                                sb-posix:map-private
                                (sb-impl::fd-stream-fd stream)
                                0))
           (sap2 (sb-posix:mmap (sb-sys:sap+ sap1 +page-size+)
                                file-length
                                sb-posix:prot-read
                                (logior sb-posix:map-private sb-posix:map-fixed)
                                (sb-impl::fd-stream-fd stream)
                                0))
           (handle (make-mmap-handle :address sap1
                                     :length map-length)))
      ;; simple-base-string header word
      (setf (sb-sys:sap-ref-word sap2 (- (* 2 sb-vm:n-word-bytes)))
            sb-vm:simple-base-string-widetag)
      ;; simple-base-string length word (as fixnum)
      (setf (sb-sys:sap-ref-word sap2 (- (* 1 sb-vm:n-word-bytes)))
            (ash file-length sb-vm:n-fixnum-tag-bits))
      (setf (mmap-handle-string handle)
            (sb-kernel:%make-lisp-obj
             (logior sb-vm:other-pointer-lowtag
                     (- (sb-sys:sap-int sap2) (* 2 sb-vm:n-word-bytes)))))
      handle)))

;;;; Implement the benchmark

(defun split-and-trim-sequence (delimiter string start-of-line end-of-line trim)
  (declare (type simple-base-string string)
           (type fixnum start-of-line end-of-line)
           (optimize speed))
  (loop for start = start-of-line then (1+ end)
     for end = (position delimiter string :start start :end end-of-line)
     for end-pos = (or end end-of-line)
     for length = (- end-pos start)
     do (loop while (< start end-pos)
	   while (eql (aref string start) trim)
	   do (incf start))
     do (loop until (eql end-pos start)
	   while (eql (aref string (1- end-pos)) trim)
	   do (decf end-pos))
     collect (if (< length 64)
		 ;; A displaced array takes around 64 bytes. For strings
		 ;; shorter than 64 base-chars we might as well just
		 ;; make a simple string.
		 (subseq string start end-pos)
		 (make-array length
			     :element-type 'base-char
			     :displaced-to string
			     :displaced-index-offset start))
     while (and end (< end end-of-line))))

(defun parse-text (filename)
  (declare (optimize speed))
  (with-mmaped-base-string (string filename)
    ;; Note that the contents of the hash-table won't be valid outside
    ;; the dynamic scope of WITH-MMAPED-BASE-STRING, since we're
    ;; displacing arrays to STRING.
    (let ((ht (make-hash-table :test 'equal)))
      (loop for start = 0 then (1+ end)
	 for end = (position #\Newline string :start start)
	 for end-pos = (or end (length string))
	 do (let ((fields (split-and-trim-sequence #\~ string start end-pos
						   #\space)))
	      (when (= (length (the list fields)) 3)
		(destructuring-bind (id attribute value) fields
		  (when (not (gethash id ht))
		    (setf (gethash id ht) (make-hash-table :test 'equal)))
		  (let ((fields-ht (gethash id ht)))
		    (setf (gethash attribute fields-ht) value)))))
	 while end)
      (print (hash-table-count ht))
      nil)))
