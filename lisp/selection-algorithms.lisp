(declaim (optimize speed))

(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

;;; based on selection sort
(defmacro select-top-k (type array k comparator)
  (let ((comparator (typecase comparator
                      (symbol comparator)
                      (list (progn
                              (assert (= (length comparator) 2))
                              (second comparator))))))
    `(let ((array ,array)
           (k ,k))
       (declare (type (simple-array ,type (*)) array)
                (type array-index k))
       (let ((n (length array)))
         (declare (type fixnum n))
         (loop for i of-type array-index to k
               for max-index of-type array-index = i
               for max-value of-type ,type = (aref array i)
               do (progn
                    (loop for j of-type array-index from (1+ i) below n
                          do (let ((current-value (aref array j)))
                               (when (,comparator current-value max-value)
                                 (setf max-index j
                                       max-value current-value))))
                    (rotatef (aref array i) (aref array max-index))))
         (aref array (1- k))))))


;;; based on quicksort
(defmacro partition (type array left right pivot-index comparator)
  (let ((comparator (typecase comparator
                      (symbol comparator)
                      (list (progn
                              (assert (= (length comparator) 2))
                              (second comparator))))))
    `(let ((array ,array)
           (left ,left)
           (right ,right)
           (pivot-index ,pivot-index))
       (declare (type (simple-array ,type (*)) array)
                (type array-index left right pivot-index))
       (rotatef (aref array pivot-index) (aref array right))
       (loop with store-index of-type array-index = left
             for i of-type array-index from left to right
             for current-value of-type ,type = (aref array i)
             do (when (,comparator current-value (aref array pivot-index))
                  (setf (aref array i) (aref array store-index)
                        (aref array store-index) current-value)
                  (incf store-index))
             finally (progn
                       (rotatef (aref array right) (aref array store-index))
                       (return store-index))))))

(defmacro select-top-k (type array left right k comparator)
  (let ((comparator (typecase comparator
                      (symbol comparator)
                      (list (progn
                              (assert (= (length comparator) 2))
                              (second comparator))))))
    `(let ((array ,array)
           (left ,left)
           (right ,right)
           (k ,k))
       (declare (type (simple-array ,type (*)) array)
                (type array-index left right k))
       (labels ((select (array left right k)
                  (declare (type (simple-array ,type (*)) array)
                           (type array-index left right k))
                  (if (= left right)
                      (aref array left)
                      (let* ((pivot-index (+ left (random (- right left))))
                             (pivot-new-index (partition ,type array left right pivot-index ,comparator))
                             (pivot-dist (1+ (- pivot-new-index left))))
                        (cond ((= pivot-dist k)
                               (aref array pivot-new-index))
                              ((< k pivot-dist)
                               (select array left (1- pivot-new-index) k))
                              (t
                               (select array (1+ pivot-new-index) right (- k pivot-dist))))))))
         (select array left right k)))))


;;; quicksort variant, only first k sorted
(defmacro select-top-k (type array left right k comparator)
  (let ((comparator (typecase comparator
                      (symbol comparator)
                      (list (progn
                              (assert (= (length comparator) 2))
                              (second comparator))))))
    `(let ((array ,array)
           (left ,left)
           (right ,right)
           (k ,k))
       (declare (type (simple-array ,type (*)) array)
                (type array-index left right k))
       (labels ((select (array left right k)
                  (declare (type (simple-array ,type (*)) array)
                           (type array-index left right k))
                  (when (> right left)
                    (let* ((pivot-index (+ left (random (- right left))))
                           (pivot-new-index (partition ,type array left right pivot-index ,comparator)))
                      (select array left (1- pivot-new-index) k)
                      (when (< pivot-new-index (+ left k))
                        (select array (1+ pivot-new-index) right k))))))
         (select array left right k)))))

;;; quicksort variant, first k not sorted
(defmacro select-top-k (type array left right k comparator)
  (let ((comparator (typecase comparator
                      (symbol comparator)
                      (list (progn
                              (assert (= (length comparator) 2))
                              (second comparator))))))
    `(let ((array ,array)
           (left ,left)
           (right ,right)
           (k ,k))
       (declare (type (simple-array ,type (*)) array)
                (type array-index left right k))
       (labels ((select (array left right k)
                  (declare (type (simple-array ,type (*)) array)
                           (type array-index left right k))
                  (when (> right left)
                    (let* ((pivot-index (+ left (random (- right left))))
                           (pivot-new-index (partition ,type array left right pivot-index ,comparator)))
                      (when (> pivot-new-index (+ left k))
                        (select array left (1- pivot-new-index) k))
                      (when (< pivot-new-index (+ left k))
                        (select array (1+ pivot-new-index) right k))))))
         (select array left right k)))))


#| test

(defvar *random-dvec-size* 1000000)
(defvar *random-dvec* (make-array *random-dvec-size* :element-type 'double-float))
(declaim (type fixnum *random-dvec-size*)
         (type (simple-array double-float (*)) *random-dvec*))
(loop for i of-type array-index below *random-dvec-size*
      do (setf (aref *random-dvec* i) (random 1d0)))

(cffi:defcfun gsl-sort-largest :int
  (dest :pointer)
  (k :uint)
  (src :pointer)
  (stride :uint)
  (n :uint))

|#
