(deftype index ()
  `(mod ,most-positive-fixnum))
(deftype positive-fixnum ()
  '(and unsigned-byte fixnum))
(deftype word ()
  'sb-vm:word)

(defstruct (hash
             (:constructor %make-hash (size scale data mix
                                            &key (hash-fun #'sxhash)
                                                 (test-fun #'eql)
                                                 (load     .5d0)
                                            &aux (length (length data))
                                                 (max-count (floor (* length 1/3 load)))
                                                 (min-count (ceiling (* length 1/3 load .4d0))))))
  (hash-fun (error "Foo") :type (function (t) (values positive-fixnum)))
  (test-fun (error "Foo") :type (function (t t) (values t)))
  (size     (error "Foo") :type index)
  (scale    (error "Foo") :type index)
  (data     (error "Foo") :type simple-vector)
  (length   (error "Foo") :type index)
  (mix      (error "Foo") :type positive-fixnum)
  (count    0             :type positive-fixnum)
  (cleared  0             :type positive-fixnum)
  (load     .5d0          :type double-float)
  (max-count 0            :type positive-fixnum)
  (min-count 0            :type positive-fixnum))

(defun recompute-load-thresholds (table)
  (declare (type hash table))
  (setf (hash-count table)   0
        (hash-cleared table) 0
        (hash-max-count table) (floor (* (hash-length table) 1/3 (hash-load table)))
        (hash-min-count table) (ceiling (* (hash-length table) 1/3 (hash-load table) .4d0)))
  table)

(defun make-mix-value ()
  (loop for mix = (random (1+ most-positive-fixnum))
        unless (< mix 3)
          do (return mix)))

(defparameter *mix* (make-mix-value))

(declaim (ftype (function (index &key (:mix (or null positive-fixnum)) (:data (or null simple-vector)))
                          (values index positive-fixnum simple-vector positive-fixnum &optional))
                magic-values))
(defun magic-values (size &key mix data)
  (declare (optimize speed))
  (values size
          (floor (* size 2 .995d0))
          #+darwin
          (fill (if (and data
                         (>= (length data) (* 3 (+ size 4))))
                    data
                    (make-array (* 3 (+ size 4))))
                0)
          #-darwin
          (if (and data
                   (>= (length data) (* 3 (+ size 4))))
              (fill data 0)
              (make-array (* 3 (+ size 4))))
          (or mix (make-mix-value))))

(defun make-hash (size &key hash test load)
  (declare (type index size))
  (when (< size 8)
    (setf size 8))
  (setf load (if load (float load 1d0) .5d0))
  (multiple-value-call #'%make-hash (magic-values (ceiling size load) :mix *mix*)
    :hash-fun (or hash #'sxhash)
    :test-fun (or test #'eql)
    :load     load))

(declaim (inline scale-by mix))
(defun scale-by (x scale)
  (declare (type (and unsigned-byte fixnum) x scale)
           (optimize speed))
  (values (sb-bignum:%multiply (sb-kernel:get-lisp-obj-address x)
                               scale)))

(defun mix (x mix)
  (declare (type (and unsigned-byte fixnum) x mix)
           (optimize speed))
  (multiple-value-bind (hi lo)
      (sb-bignum:%multiply x (sb-kernel:get-lisp-obj-address mix))
    (logand (1- (ash 1 64))
            (- lo hi))))

(defconstant +empty+ '+empty+)

(declaim (inline %hash-get))
(defun %hash-get (table key)
  (declare (type hash table)
           (optimize speed (safety 0)))
  (let* ((hash  (logior (logand (mix (funcall (hash-hash-fun table) key)
                                     (hash-mix table))
                                most-positive-fixnum)
                        1))
         (len   (hash-length table))
         (scale (hash-scale  table))
         (data  (hash-data   table))
         (test  (hash-test-fun table))
         (loc   (* 3 (scale-by hash scale))))
    (declare (type (and unsigned-byte fixnum) loc))
    (loop for i from loc below len by 3
          do
          (let ((h2 (aref data i)))
            (declare (type (and unsigned-byte fixnum) h2))
            (cond ((zerop h2)
                   (return (values nil nil)))
                  ((< h2 hash))
                  ((and (eql h2 hash)
                        (funcall test key (aref data (1+ i))))
                   (let ((value (aref data (+ i 2))))
                     (return (if (eql value +empty+)
                                 (values nil nil)
                                 (values value t)))))
                  (t (return (values nil nil))))))))

(defun hash-get (table key)
  (check-type table hash)
  (%hash-get table key))

(declaim (inline %hash-clear))
(defun %hash-clear (table key)
  (declare (type hash table)
           (optimize speed (safety 0)))
  (let* ((hash  (logior (logand (mix (funcall (hash-hash-fun table) key)
                                     (hash-mix table))
                                most-positive-fixnum)
                        1))
         (len   (hash-length table))
         (scale (hash-scale  table))
         (data  (hash-data   table))
         (test  (hash-test-fun table))
         (loc   (* 3 (scale-by hash scale))))
    (declare (type (and unsigned-byte fixnum) loc))
    (loop for i from loc below len by 3
          do
          (let ((h2 (aref data i)))
            (declare (type (and unsigned-byte fixnum) h2))
            (cond ((zerop h2)
                   (return (values nil nil)))
                  ((< h2 hash))
                  ((and (eql h2 hash)
                        (funcall test key (aref data (1+ i))))
                   (return (values (shiftf (aref data (+ i 2)) +empty+)
                                   t)))
                  (t (return (values nil nil))))))))

(defun hash-clear (table key)
  (check-type table hash)
  (multiple-value-bind (old-value foundp) (%hash-clear table key)
    (when (and foundp
               (>= (incf (hash-cleared table)) (hash-min-count table)))
      (hash-resize table (hash-size table)))
    (values old-value foundp)))

(declaim (inline %hash-insert))
(defun %hash-insert (table key value)
  (declare (type hash table)
           (optimize speed (safety 0)))
  (let* ((hash  (logior (logand (mix (funcall (hash-hash-fun table) key)
                                     (hash-mix table))
                                most-positive-fixnum)
                        1))
         (len   (hash-length table))
         (scale (hash-scale  table))
         (data  (hash-data   table))
         (test  (hash-test-fun table))
         (loc   (* 3 (scale-by hash scale))))
    (declare (type (and unsigned-byte fixnum) loc))
    (loop for i from loc below len by 3
          do
          (let ((h2 (aref data i)))
            (declare (type (and unsigned-byte fixnum) h2))
            (cond ((zerop h2) ; empty slot
                   (setf (aref data      i)  hash
                         (aref data (1+ i))  key
                         (aref data (+ i 2)) value)
                   (return-from %hash-insert 1))
                  ((< h2 hash))
                  ((and (eql h2 hash)
                        (funcall test key (aref data (1+ i))))
                   (setf (aref data (+ i 2)) value)
                   (return-from %hash-insert 0))
                  (t ; only remaining case
                   (loop for j from (+ i 3) below len by 3
                         do (when (eql 0 (aref data j))
                              (loop for k from (- j 3) downto i by 3
                                    do (setf (aref data (+ k 3)) (aref data k)
                                             (aref data (+ k 4)) (aref data (1+ k))
                                             (aref data (+ k 5)) (aref data (+ k 2))))
                              (setf (aref data i) hash
                                    (aref data (1+ i)) key
                                    (aref data (+ i 2)) value)
                              (return-from %hash-insert 1)))
                   (return-from %hash-insert nil)))))
    nil))

(defun try-hash-resize (table new-size)
  (declare (type index new-size))
  (check-type table hash)
  (multiple-value-bind (size scale data)
      (magic-values new-size :mix (hash-mix table))
    (let* ((old-data (hash-data table))
           (old-len  (hash-length table))
           (len      (length data))
           (next-loc 0)  ; next free loc
           (count    0))
      (declare (type positive-fixnum next-loc count)
               (optimize speed (safety 0)))
      (loop for i from 0 below old-len by 3
            do
            (let ((hash (aref old-data i))
                  (key  (aref old-data (1+ i)))
                  (val  (aref old-data (+ i 2))))
              (unless (or (eql 0 hash)
                          (eql +empty+ val))
                (incf count)
                (let ((loc (max next-loc
                                (truly-the index (* 3 (scale-by hash scale))))))
                  (declare (type index loc))
                  (when (>= loc len)
                    (return-from try-hash-resize nil))
                  (setf (aref data       loc) hash
                        (aref data  (1+ loc)) key
                        (aref data (+ 2 loc)) val
                        next-loc              (+ 3 loc))))))
      (setf (hash-size   table) size
            (hash-scale  table) scale
            (hash-data   table) data
            (hash-length table) len
            (hash-cleared table) 0)
      (recompute-load-thresholds table)
      (setf (hash-count  table) count)
      table)))

(defun try-hash-rehash (table new-table)
  (declare (type hash table new-table))
  (let ((data (hash-data   table))
        (len  (hash-length table))
        (count 0))
    (declare (optimize speed (safety 0))
             (type index count))
    (loop for i from 0 below len by 3
          do
          (let ((hash  (aref data i))
                (key   (aref data (1+ i)))
                (value (aref data (+ i 2))))
            (unless (or (eql hash 0)
                        (eql value +empty+))
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
    (setf (values (hash-size table) (hash-scale table)
                  (hash-data table) (hash-mix   table))
          (magic-values new-size))
    (recompute-load-thresholds new-table)
    (loop
      (unless (try-hash-rehash table new-table)
        (setf (values (hash-size table) (hash-scale table)
                      (hash-data table) (hash-mix   table))
              (magic-values new-size :data (hash-data new-table)))))
    new-table))

(defun hash-resize (table new-size)
  (or (try-hash-resize table new-size)
      (hash-rehash table new-size)))

(defun hash-insert (table key value)
  (check-type table hash)
  (loop
    (let ((increment (%hash-insert table key value)))
      (cond (increment
             (when (>= (incf (hash-count table) (truly-the bit increment)) (hash-max-count table))
               (hash-resize table (* 2 (hash-size table))))
             (return value))
            (t
             (hash-resize table (* 2 (hash-size table))))))))

(declaim (inline (setf hash-get)))
(defun (setf hash-get) (value table key)
  (hash-insert table key value))
