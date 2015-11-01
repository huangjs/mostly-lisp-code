(deftype index ()
  `(mod ,most-positive-fixnum))

(deftype digit ()
  'sb-bignum:bignum-element-type)

(defstruct (bignum-vector
             (:constructor %make-bignum-vector))
  (size   (error "Must provide size")
          :type index)
  (digits (error "Must provide digits vector")
          :type (simple-array (simple-array digit 1) 1)))

(declaim (inline next-even))
(defun next-even (x)
  (declare (fixnum x))
  (logandc2 (1+ x) 1))

(defparameter *bignum-vector-cache* (make-hash-table :test #'eql :synchronized t))

(declaim (ftype (function (index) (values (or null (simple-array digit 1)) &optional))
                get-vector))
(defun get-vector (n)
  (let ((table *bignum-vector-cache*))
    (sb-ext:with-locked-hash-table (table)
      (let ((vector (pop (gethash n table))))
        (declare (optimize speed))
        (and vector
             (fill (the (simple-array digit 1) vector)
                   0))))))

(declaim (inline make-digit-vector))
(defun make-digit-vector (n)
  (let ((n (next-even n)))
    (the (simple-array digit 1)
      (or (get-vector n)
          (make-array n :element-type 'digit :initial-element 0)))))

(defun free-vector (vector)
  (declare (type (simple-array digit 1) vector))
  (let ((table *bignum-vector-cache*))
    (sb-ext:with-locked-hash-table (table)
      (push vector (gethash (length vector) table)))
    nil))

(defun make-bignum-vector (n)
  (%make-bignum-vector
   :size n
   :digits (make-array
            1
            :initial-element (make-digit-vector n))))

(defun init-bignum-vector (&rest numbers)
  (assert (every (lambda (x)
                   (typep x 'digit))
                 numbers))
  (let* ((n (length numbers))
         (vector (make-digit-vector n)))
    (%make-bignum-vector
     :size n
     :digits (make-array
              1
              :initial-element
              (replace vector numbers)))))

(defun %add-digit-vectors (x y carry dst size)
  (declare (type (simple-array digit 1) x y carry dst)
           (type index size)
           (optimize speed (safety 0)))
  (do ((i (- (next-even size) 2) (- i 2))
       (j 0))
      ((< i 0) dst)
    (declare (type index j))
    (setf j i)
    (let* ((i     j)
           (i+1   (1+ i))
           (x_i   (aref x i))
           (x_i+1 (aref x i+1))
           (y_i   (aref y i))
           (y_i+1 (aref y i+1))
           (c_i   (aref carry i))
           (c_i+1 (aref carry i+1)))
      (multiple-value-bind (lo0 hi0)
          (sb-bignum:%add-with-carry x_i y_i c_i)
        (multiple-value-bind (lo1 hi1)
            (sb-bignum:%add-with-carry x_i+1 y_i+1 c_i+1)
          (setf (aref dst i)     lo0
                (aref dst i+1)   lo1
                (aref carry i)   hi0
                (aref carry i+1) hi1))))))

(defun %propagate-sign-bits (x y written carry size)
  "Common case: (len x) = (len y)"
  (declare (type (simple-array digit 1) x y written carry)
           (type index size)
           (optimize speed (safety 0)))
  (let ((delta 0))
    (declare (type digit delta))
    (do ((i (- (next-even size) 2) (- i 2))
         (j 0))
        ((< i 0)
           (if (zerop delta)
               (prog1 nil
                 (free-vector carry))
               carry))
      (declare (type index j))
      (setf j i)
      (let* ((i       j)
             (i+1     (1+ i))
             (x_i     (aref x i))
             (x_i+1   (aref x i+1))
             (xs_i    (sb-bignum:%ashr x_i   (1- sb-vm:n-word-bits)))
                        
             (y_i     (aref y i))
             (y_i+1   (aref y i+1))
             (ys_i    (sb-bignum:%ashr y_i   (1- sb-vm:n-word-bits)))
                        
             (c_i     (aref carry i))
             (out_i   (sb-bignum:%add-with-carry xs_i   ys_i   c_i))

             (c_i+1   (aref carry i+1))
             (xs_i+1  (sb-bignum:%ashr x_i+1 (1- sb-vm:n-word-bits)))
             (ys_i+1  (sb-bignum:%ashr y_i+1 (1- sb-vm:n-word-bits)))
             (out_i+1 (sb-bignum:%add-with-carry xs_i+1 ys_i+1 c_i+1))
                        
             (w_i    (aref written i))
             (w_i+1  (aref written i+1))
             (sg_i   (sb-bignum:%ashr w_i   (1- sb-vm:n-word-bits)))
             (sg_i+1 (sb-bignum:%ashr w_i+1 (1- sb-vm:n-word-bits))))
        (setf (aref carry i)   out_i
              (aref carry i+1) out_i+1
              delta            (logior delta
                                       (logxor sg_i   out_i)
                                       (logxor sg_i+1 out_i+1)))))))

(defun %%propagate-sign-bit-with-sign (x sign written carry size)
  "sign must only contain 0 or -1U"
  (declare (type (simple-array digit 1) x sign written carry)
           (type index size)
           (optimize speed (safety 0)))
  (let ((delta 0))
    (declare (type digit delta))
    (do ((i (- (next-even size) 2) (- i 2))
         (j 0))
        ((< i 0)
           (if (zerop delta)
               (prog1 nil
                 (free-vector carry))
               carry))
      (declare (type index j))
      (setf j i)
      (let* ((i       j)
             (i+1     (1+ i))
             (x_i     (aref x i))
             (x_i+1   (aref x i+1))
             (xs_i    (sb-bignum:%ashr x_i   (1- sb-vm:n-word-bits)))
                        
             (ys_i     (aref sign i))
             (ys_i+1   (aref sign i+1))
                        
             (c_i     (aref carry i))
             (out_i   (sb-bignum:%add-with-carry xs_i   ys_i   c_i))

             (c_i+1   (aref carry i+1))
             (xs_i+1  (sb-bignum:%ashr x_i+1 (1- sb-vm:n-word-bits)))
             (out_i+1 (sb-bignum:%add-with-carry xs_i+1 ys_i+1 c_i+1))
                        
             (w_i    (aref written i))
             (w_i+1  (aref written i+1))
             (sg_i   (sb-bignum:%ashr w_i   (1- sb-vm:n-word-bits)))
             (sg_i+1 (sb-bignum:%ashr w_i+1 (1- sb-vm:n-word-bits))))
        (setf (aref carry i)   out_i
              (aref carry i+1) out_i+1
              delta            (logior delta
                                       (logxor sg_i   out_i)
                                       (logxor sg_i+1 out_i+1)))))))

(defun redundant-digits-p (msb remainder size)
  (declare (type (simple-array digit 1) msb remainder)
           (type index size)
           (optimize speed (safety 0)))
  (let ((delta 0))
    (declare (type digit delta))
    (do ((i (- (next-even size) 2) (- i 2))
         (j 0))
        ((< i 0) (zerop delta))
      (declare (type index j))
      (setf j i)
      (let* ((i       j)
             (i+1     (1+ j))
             (msb_i   (aref msb i))
             (msb_i+1 (aref msb i+1))
             (rem_i   (aref remainder i))
             (rem_i+1 (aref remainder i+1))
             (sg_i    (sb-bignum:%ashr rem_i   (1- sb-vm:n-word-bits)))
             (sg_i+1  (sb-bignum:%ashr rem_i+1 (1- sb-vm:n-word-bits))))
        (setf delta (logior delta
                            (logxor msb_i sg_i)
                            (logxor msb_i+1 sg_i+1)))))))

(defun compute-sign (x size)
  (declare (type (simple-array digit 1) x)
           (type index size)
           (optimize speed (safety 0)))
  (let ((dst (make-digit-vector size)))
    (do ((i (- (next-even size) 2) (- i 2))
         (j 0))
        ((< i 0) dst)
      (declare (type index j))
      (setf j i)
      (let* ((i      j)
             (i+1    (1+ i))
             (x_i    (aref x i))
             (x_i+1  (aref x i+1))
             (sg_i   (sb-bignum:%ashr x_i   (1- sb-vm:n-word-bits)))
             (sg_i+1 (sb-bignum:%ashr x_i+1 (1- sb-vm:n-word-bits))))
        (setf (aref dst i)      sg_i
              (aref dst i+1)    sg_i+1)))))

;; TODO: Tile the other way to only use fixed, cache-line-sized temporaries.
(defun add-bignum-vectors (x y)
  (declare (type bignum-vector x y)
           (optimize speed))
  (let* ((size     (min (bignum-vector-size x)
                        (bignum-vector-size y)))
         (carry    (make-digit-vector size))
         (vectors '()))
    (declare (type list vectors))
    (when (< (length (bignum-vector-digits x)) (length (bignum-vector-digits y)))
      (rotatef x y))
    ;; Step 1: Add digits (lsb first)
    (do ((digit_x (1- (length (bignum-vector-digits x)))
             (sb-ext:truly-the fixnum (1- digit_x)))
           (digit_y (1- (length (bignum-vector-digits y))) (1- digit_y)))
        ((< digit_y 0))
      (push (%add-digit-vectors (aref (bignum-vector-digits x) digit_x)
                                (aref (bignum-vector-digits y) digit_y)
                                carry
                                (make-digit-vector size)
                                size)
            vectors))
    ;; Step 2: propagate carry across the remainder of x
    (flet ((finish-addition (sign-bit)
             (if sign-bit
                 (push sign-bit vectors)
                 (loop for (first second) on vectors by #'cdr
                       while (and second
                                  (redundant-digits-p (first  vectors)
                                                      (second vectors)
                                            size))
                       do (free-vector (pop vectors))))))
      (let ((delta-length (- (length (bignum-vector-digits x))
                             (length (bignum-vector-digits y)))))
        (cond ((zerop delta-length)
               (finish-addition (%propagate-sign-bits
                                 (aref (bignum-vector-digits x) 0)
                                 (aref (bignum-vector-digits y) 0)
                                 (car vectors)
                                 carry
                                 size)))
              (t
               (let ((sign (compute-sign (aref (bignum-vector-digits y) 0) size)))
                 (do ((digit (1- delta-length) (- digit 1)))
                     ((< digit 0))
                   (push (%add-digit-vectors (aref (bignum-vector-digits x) digit)
                                             sign
                                             carry
                                             (make-digit-vector size)
                                             size)
                         vectors))
                 (finish-addition (%%propagate-sign-bit-with-sign
                                   (aref (bignum-vector-digits x) 0)
                                   sign
                                   (car vectors)
                                   carry
                                   size))
                 (free-vector sign))))))
    (%make-bignum-vector :size size
                         :digits (make-array (length vectors) :initial-contents vectors))))

(defun naive-add (x y)
  (declare (optimize speed (safety 0))
           (type (simple-array integer 1) x y))
  (map '(simple-array integer 1)
       (lambda (x y)
         (declare (type integer x y)
                  (muffle-conditions t))
         (+ x y))
       x y))
