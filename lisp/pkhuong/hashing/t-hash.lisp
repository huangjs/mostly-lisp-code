(defpackage "T-HASH"
    (:use "CL" "SB-BIGNUM" "SB-EXT")
  (:export "MAKE" "GET" "SET" "CLEAR" "RESET" "HASH")
  (:nicknames "H")
  (:shadow "GET" "SET"))
(in-package "T-HASH")

(deftype index ()
  `(mod ,most-positive-fixnum))
(deftype positive-fixnum ()
  '(and unsigned-byte fixnum))
(deftype word ()
  'sb-vm:word)
(deftype hash-vector ()
  '(simple-array t 1))

;; data vector contains all the essential info:
;; mix value, scale value, length (obviously) and the entries.
(defstruct (hash
             (:constructor %make-hash (size data
                                            &key (hash-fun #'sxhash) (test-fun #'eql)
                                                 (count 0) (cleared 0) (load .5d0)
                                            &aux (max-count (floor (* size load)))
                                                 (min-count (ceiling (* size load .4d0))))))
  (hash-fun (error "Foo") :type (function (t) (values positive-fixnum &optional)))
  (test-fun (error "Foo") :type (function (t t) (values t &optional)))
  (size     0             :type index)
  (data     (error "Foo") :type hash-vector)
  (count    0             :type positive-fixnum) ; number of filled entries
  (cleared  0             :type positive-fixnum) ; number of entries marked as deleted
  (load     .5d0          :type double-float)
  (max-count 0            :type positive-fixnum)
  (min-count 0            :type positive-fixnum))

(defun recompute-load-thresholds (table)
  (declare (type hash table))
  (let ((entries (1- (floor (length (hash-data table)) 3)))
        (load    (hash-load table)))
    (setf (hash-count table)   0
          (hash-cleared table) 0
          (hash-max-count table) (floor (* entries load))
          (hash-min-count table) (ceiling (* entries load .4d0))))
  table)

(defun make-mix-value ()
  (loop for mix = (random (1+ most-positive-fixnum))
        when (and (< (truncate (integer-length most-positive-fixnum) 4)
                               (logcount mix)
                               (ceiling (* 3 (integer-length most-positive-fixnum)) 4))
                  (>= mix (isqrt most-positive-fixnum)))
          do (return mix)))

(defparameter *mix* (make-mix-value))

(declaim (ftype (function (index &key
                                 (:mix (or null positive-fixnum))
                                 (:data (or null hash-vector)))
                          (values index hash-vector))
                magic-values))
(defun magic-values (size &key mix data)
  (declare (optimize speed))
  (let* ((mix   (or mix (make-mix-value)))
         (scale (floor (* size 2 .999d0)))
         (len  (* 3 (+ size 5)))
         (data (if (and data
                       (typep data 'simple-vector)
                       (>= (length data) len))
                  (fill data 0)
                  (make-array len))))
    (declare (type positive-fixnum mix scale len)
             (type hash-vector data))
    (setf (aref data 0) mix
          (aref data 1) scale
          (aref data 2) len)
    (values size data)))

(defun make (size &key load hash test)
  (declare (type index size))
  (when (< size 8)
    (setf size 8))
  (setf load (if load (float load 1d0) .5d0))
  (multiple-value-call #'%make-hash (magic-values (ceiling size load) :mix *mix*)
    :load     load
    :hash-fun (or hash #'sxhash)
    :test-fun (or test #'eql)))

(defun reset (hash)
  (declare (type hash hash))
  (fill (hash-data hash) 0 :start 3)
  (setf (hash-count hash) 0
        (hash-cleared hash) 0)
  hash)

(declaim (inline scale-by mix))
(defun scale-by (x scale)
  (declare (type positive-fixnum x scale)
           (optimize speed))
  (values (sb-bignum:%multiply (sb-kernel:get-lisp-obj-address x)
                               scale)))

(defun mix (x mix)
  (declare (type positive-fixnum x mix)
           (optimize speed))
  (multiple-value-bind (hi lo)
      (sb-bignum:%multiply x (sb-kernel:get-lisp-obj-address mix))
    (logand (1- (ash 1 64))
            (- lo hi))))

(declaim (inline hash-info)
         (ftype (function (hash hash-vector fixnum &optional (or null positive-fixnum))
                          (values fixnum index &optional))
                hash-info))
(defun hash-info (table data key &optional hash)
  (declare (type hash table)
           (type hash-vector data)
           (optimize speed))
  (let* ((mix   (aref data 0))
         (scale (aref data 1))
         (hash  (or hash (logior (logand (mix (funcall (hash-hash-fun table) key) mix)
                                         most-positive-fixnum)
                                 1)))
         (loc   (* 3 (truly-the positive-fixnum (1+ (scale-by hash scale))))))
    (declare (type positive-fixnum mix scale loc))
    (values hash loc)))

(declaim (inline %hash-get))
(defun %hash-get (table key)
  (declare (type hash table)
           (optimize speed (safety 0)))
  (let* ((data  (hash-data table))
         (len   (aref data 2))
         (test  (hash-test-fun table)))
    (multiple-value-bind (hash loc) (hash-info table data key)
      (loop for i from loc below len by 3
            do
         (let ((h2 (aref data i)))
           (declare (type fixnum h2))
           (cond ((zerop h2)
                  (return (values nil nil)))
                 ((< h2 hash))
                 ((and (eql h2 hash)
                       (funcall test key (aref data (1+ i))))
                  (let ((value (aref data (+ i 2))))
                    (return (values value t))))
                 (t (return (values nil nil)))))))))

(defun get (table key)
  (check-type table hash)
  (%hash-get table key))

(defconstant +deleted+ '+deleted+)

(declaim (inline %hash-clear))
(defun %hash-clear (table key)
  (declare (type hash table)
           (optimize speed (safety 0)))
  (let* ((data  (hash-data table))
         (len   (aref data 2))
         (test  (hash-test-fun table)))
    (multiple-value-bind (hash loc) (hash-info table data key)
      (loop for i from loc below len by 3
            do
         (let ((h2 (aref data i)))
           (declare (type fixnum h2))
           (cond ((zerop h2)
                  (return (values nil nil)))
                 ((< h2 hash))
                 ((and (eql h2 hash)
                       (funcall test key (aref data (1+ i))))
                  (let ((value (aref data (+ i 2))))
                    (setf (aref data (1+ i)   +deleted+)
                          (aref data (+ i 2)) +deleted+)))
                 (t (return (values nil nil)))))))))

(defun clear (table key)
  (check-type table hash)
  (multiple-value-bind (old-value foundp) (%hash-clear table key)
    (when (and foundp
               (< (- (hash-count table) (incf (hash-cleared table))) (hash-min-count table)))
      (hash-resize table (if (< (hash-count table) (hash-min-count table))
                             (truncate (* 2 (hash-size table)) 3)
                             (hash-size table))))
    (values old-value foundp)))

(declaim (inline %hash-insert))
(defun %hash-insert (table key value)
  (declare (type hash table)
           (optimize speed (safety 0)))
  (let* ((data  (hash-data table))
         (len   (aref data 2))
         (test  (hash-test-fun table)))
    (multiple-value-bind (hash loc) (hash-info table data key)
      (loop for i from loc below len by 3
            do
         (let ((h2 (aref data i)))
           (declare (type fixnum h2))
           (cond ((zerop h2)            ; empty slot
                  (setf (aref data      i)  hash
                        (aref data (1+ i))  key
                        (aref data (+ i 2)) value)
                  (return-from %hash-insert 1))
                 ((< h2 hash))
                 ((and (eql h2 hash)
                       (funcall test key (aref data (1+ i))))
                  (setf (aref data (+ i 2)) value)
                  (return-from %hash-insert 0))
                 ((eql (aref data (+ i 2)) +deleted+)
                  (setf (aref data       i) hash
                        (aref data  (1+ i)) key
                        (aref data (+ i 2)) value)
                  (return-from %hash-insert 0))
                 (t                     ; only remaining case
                  (loop for j from (+ i 3) below len by 3
                        do (let ((h3 (aref data j))
                                 (v3 (aref data (+ 2 j))))
                             (declare (type fixnum h3))
                             (when (or (zerop h3)
                                       (eql v3 +deleted+))
                               (loop for k from (- j 3) downto i by 3
                                     do (setf (aref data (+ k 3)) (aref data k)
                                              (aref data (+ k 4)) (aref data (1+ k))
                                              (aref data (+ k 5)) (aref data (+ k 2))))
                               (setf (aref data i) hash
                                     (aref data (1+ i)) key
                                     (aref data (+ i 2)) value)
                               (return-from %hash-insert (if (zerop h3) 1 0)))))
                  (return-from %hash-insert nil))))))
    nil))

(defun try-hash-resize (table new-size)
  (declare (type index new-size))
  (check-type table hash)
  (multiple-value-bind (size data)
      (magic-values new-size :mix (aref (hash-data table) 0))
    (let* ((old-data (hash-data table))
           (old-len  (length     old-data))
           (scale    (aref data 1))
           (len      (aref data 2))
           (next-loc 3)  ; next free loc
           (count    0))
      (declare (type positive-fixnum next-loc count)
               (optimize speed (safety 0)))
      (loop for i from 3 below old-len by 3
            do
            (let ((hash (aref old-data i))
                  (key  (aref old-data (1+ i)))
                  (val  (aref old-data (+ i 2))))
              (declare (type fixnum hash))
              (unless (or (zerop hash)
                          (eql +deleted+ val))
                (incf count)
                (let ((loc (max next-loc
                                (truly-the index (* 3 (truly-the index
                                                                 (1+ (scale-by hash scale))))))))
                  (declare (type index loc))
                  (when (>= loc len)
                    (return-from try-hash-resize nil))
                  (setf (aref data       loc) hash
                        (aref data  (1+ loc)) key
                        (aref data (+ 2 loc)) val
                        next-loc              (+ 3 loc))))))
      (setf (hash-size   table) size
            (hash-data   table) data
            (hash-cleared table) 0)
      (recompute-load-thresholds table)
      (setf (hash-count  table) count)
      table)))

(defun try-hash-rehash (table new-table)
  (declare (type hash table new-table))
  (let* ((data (hash-data   table))
         (len  (aref data 2))
         (count 0))
    (declare (optimize speed (safety 0))
             (type index count))
    (loop for i from 3 below len by 3
          do
          (let ((hash  (aref data i))
                (key   (aref data (1+ i)))
                (value (aref data (+ i 2))))
            (declare (type fixnum hash))
            (unless (or (zerop hash)
                        (eql value +deleted+))
              (incf count)
              (unless (%hash-insert new-table key value)
                (return-from try-hash-rehash nil)))))
    (setf (hash-count new-table) count
          (hash-cleared new-table) 0)
    new-table))

(defun hash-rehash (table new-size)
  (declare (type positive-fixnum new-size))
  (check-type table hash)
  (let ((new-table (copy-hash table)))
    (declare (optimize speed))
    (setf (values (hash-size table) (hash-data table))
          (magic-values new-size))
    (recompute-load-thresholds new-table)
    (loop
      (unless (try-hash-rehash table new-table)
        (setf (values (hash-size table) (hash-data table))
              (magic-values new-size :data (hash-data new-table)))))
    new-table))

(defun hash-resize (table new-size)
  (or (try-hash-resize table new-size)
      (hash-rehash table new-size)))

(defun set (table key value)
  (check-type table hash)
  (loop
    (let ((increment (%hash-insert table key value)))
      (cond (increment
             (when (>= (incf (hash-count table) (truly-the bit increment)) (hash-max-count table))
               (hash-resize table (if (>= (- (hash-count table) (hash-cleared table))
                                          (hash-max-count table))
                                      (floor (* 3 (hash-size table)) 2)
                                      (hash-size table))))
             (return value))
            (t
             (hash-resize table (floor (* 3 (hash-size table)) 2)))))))

(declaim (inline (setf get)))
(defun (setf get) (value table key)
  (set table key value))
