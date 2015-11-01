#|
Quick throw-away code for Interlude: Numerical experiments in hashing,
http://www.pvk.ca/Blog/numerical_experiments_in_hashing.html


*** These notes aren't completely up to date ***

2 sets of simulators:

1. Bucket counters:

 Simulate infinite buckets for both regular hashing and 2-left hashing,
 reports the histogram of bucket occupancy -> count

2. Finite bucket with bounded open addressing:

 Simulate buckets of bounded size with bounded linear open addressing
 for both regular hashing and 2-left hashing.

 Reports three histograms:
  - % of target insertion count
  - probe depth count
  - bucket occupancy count

Regular open addressing is obtained with bucket size = 1.
Regular bucketted search is obtained with probe depth = 1.

Regular 2-left hashing is obtained with probe depth = 1.

I suggest an inbetween with both probe depth and bucket size > 1
and 2 tables.

Test w/ stash!

(run-n-simulations (lambda ()
                     (simulate-2-open-addressing
                      (* 1024 1024 1/8)
                      (* 1024 1024)
                      3 4))
                   1000
                   (lambda (x y)
                     (mapcar (lambda (x y)
                               (free-ub64-vector x)
                               (merge-histograms x y y))
                             x y)))

(run-n-simulations (lambda ()
                     (simulate-bucket
                      (* 1024 1024 1)
                      (* 1024 1024 1)))
                   1000
                   (lambda (x y)
                     (free-ub64-vector x)
                     (merge-histograms x y y)))
|#

(macrolet
    ((def-pool (name type)
         (let ((parameter (intern (format nil "*~A-VECTORS*" name)))
               (getter    (intern (format nil "GET-~A-VECTOR" name)))
               (freeer    (intern (format nil "FREE-~A-VECTOR" name))))
           `(progn
              (defparameter ,parameter nil)
              
              (declaim (ftype (function ((and unsigned-byte fixnum))
                                        (values (simple-array ,type 1) &optional))
                              ,getter))
              (defun ,getter (n)
                (declare (type (and unsigned-byte fixnum) n)
                         (optimize speed))
                (when ,parameter
                  (loop for head on ,parameter by #'cdr
                        when (and (cdr head)
                                  (= n (length (the simple-array (second head)))))
                          do (return-from ,getter
                               (fill (the (simple-array ,type 1)
                                       (pop (cdr head)))
                                     0))))
                (make-array n :element-type ',type))

              (defun ,freeer (vector)
                (declare (type (simple-array ,type 1) vector))
                (when ,parameter
                  (push vector (cdr ,parameter)))
                (values))))))
  (def-pool ub64 (unsigned-byte 64))
  (def-pool ub32 (unsigned-byte 32))
  (def-pool ub8  (unsigned-byte  8)))

(defun print-matrix (matrix)
  (let ((width (array-dimension matrix 0))
        (height (array-dimension matrix 1))
        (*read-default-float-format* 'double-float))
    (dotimes (j height)
      (dotimes (i width (format t "~%"))
        (format t "~F " (float (aref matrix i j) 1d0))))))

(defun concatenate-columns (columns)
  (let* ((height (reduce #'max columns :key #'length))
         (matrix (make-array `(,(length columns) ,height)
                             :initial-element 0)))
    (loop for column in columns
          for i upfrom 0
          do (loop for val across column
                   for j upfrom 0
                   do (setf (aref matrix i j) val))
          finally (return matrix))))

(defun histogram->percent (histogram)
  (let ((sum (/ 1d0 (reduce #'+ histogram))))
    (map 'vector (lambda (x)
                   (* 100 (* x sum)))
         histogram)))

(defun histogram (buckets)
  (declare (type (simple-array (unsigned-byte 32) 1) buckets)
           (optimize speed))
  (let* ((counts    (get-ub64-vector 16))
         (max-count (length counts)))
    (declare (type (and unsigned-byte fixnum) max-count)
             (type (simple-array (unsigned-byte 64) 1) counts)
             (optimize (safety 0)))
    (map nil (lambda (count)
               (when (>= count max-count)
                 (free-ub64-vector counts)
                 (setf counts    (replace (get-ub64-vector (* 2 max-count))
                                          counts)
                       max-count (* 2 max-count)))
               (incf (aref counts count)))
         buckets)
    counts))

(defun merge-histograms (x y
                         &optional (dst (make-array (max (length x)
                                                         (length y))
                                                    :element-type
                                                    '(unsigned-byte 64))))
  (declare (type (simple-array (unsigned-byte 64) 1) x y dst)
           (optimize speed))
  (when (> (length x) (length y))
    (rotatef x y))
  (when (< (length dst) (length y))
    (free-ub64-vector dst)
    (setf dst (get-ub64-vector (length y))))
  (replace dst y)
  (locally (declare (optimize (safety 0)))
    (map-into dst (lambda (x y)
                    (mod (+ x y) (ash 1 64)))
              x dst)))

(defun simulate-bucket (nbuckets nitems
                        &optional (random-state *random-state*))
  (declare (type (unsigned-byte 32) nbuckets nitems)
           (type random-state random-state)
           (optimize speed))
  (let ((counts (get-ub32-vector nbuckets))
        (mask   (1- nbuckets)))
    (declare (type (unsigned-byte 32) mask)
             (optimize (safety 0)))
    (dotimes (i nitems (prog1
                           (histogram counts)
                         (free-ub32-vector counts)))
      (incf (aref counts (logand (random (ash 1 32) random-state)
                                 mask))))))

(defun simulate-2-left (nbuckets nitems
                        &optional (random-state *random-state*))
  (declare (type (unsigned-byte 32) nbuckets nitems)
           (type random-state random-state)
           (optimize speed))
  (let ((count1 (get-ub32-vector nbuckets))
        (count2 (get-ub32-vector nbuckets))
        (mask   (1- nbuckets)))
    (declare (type (unsigned-byte 32) mask)
             (optimize (safety 0)))
    (dotimes (i nitems (prog1
                           (merge-histograms (histogram count1)
                                             (histogram count2))
                         (free-ub32-vector count1)
                         (free-ub32-vector count2)))
      (let* ((h1 (logand (random (ash 1 32) random-state)
                         mask))
             (h2 (logand (random (ash 1 32) random-state)
                         mask))
             (c1 (aref count1 h1))
             (c2 (aref count2 h2)))
        (if (< c2 c1)
            (setf (aref count2 h2) (1+ c2))
            (setf (aref count1 h1) (1+ c1)))))))

(defun make-singleton-histogram (n)
  (let ((array (get-ub64-vector (1+ n))))
    (setf (aref array n) 1)
    array))

(defun simulate-open-addressing (size nitems &optional
                                 (probe-depth 1)
                                 (bucket-size 1)
                                 (max-stash 0)
                                 (random-state *random-state*))
  (declare (type (and unsigned-byte fixnum)
                 size nitems probe-depth bucket-size max-stash)
           (type random-state random-state)
           (optimize speed))
  (let ((items (get-ub32-vector size))
        (depth (get-ub64-vector probe-depth))
        (mask  (1- size))
        (stash-size 0))
    (declare (type (and fixnum unsigned-byte) stash-size))
    (flet ((leave (n)
             (declare (type (and unsigned-byte fixnum) n))
             (let ((load (round (* n 100) nitems)))
               (declare (ignorable load))
               (free-ub32-vector items)
               (return-from simulate-open-addressing
                 (list (make-singleton-histogram load)
                       (make-singleton-histogram stash-size)
                       depth
                       (histogram items))))))
      (dotimes (i nitems (leave nitems))
        (let ((hash (logand mask (random (ash 1 32) random-state))))
          (declare (optimize (safety 0)))
          (dotimes (j probe-depth (if (< stash-size max-stash)
                                      (incf stash-size)
                                      (leave i)))
            (when (< (aref items (logand mask (+ hash j)))
                     bucket-size)
              (incf (aref items (+ hash j)))
              (incf (aref depth j))
              (return))))))))

(defun simulate-2-open-addressing (size nitems &optional
                                   (probe-depth 1)
                                   (bucket-size 1)
                                   (max-stash 0)
                                   (random-state *random-state*))
  (declare (type (and unsigned-byte fixnum)
                 size nitems probe-depth bucket-size max-stash)
           (type random-state random-state)
           (optimize speed))
  (let ((table1 (get-ub32-vector size))
        (table2 (get-ub32-vector size))
        (depth  (get-ub64-vector probe-depth))
        (mask  (1- size))
        (stash-size 0))
    (declare (type (and fixnum unsigned-byte) stash-size))
    (flet ((leave (n)
             (declare (type (and unsigned-byte fixnum) n))
             (let ((load (round (* n 100) nitems)))
               (declare (ignorable load))
               (free-ub32-vector table1)
               (free-ub32-vector table2)
               (return-from simulate-2-open-addressing
                 (list (make-singleton-histogram load)
                       (make-singleton-histogram stash-size)
                       depth
                       (merge-histograms (histogram table1)
                                         (histogram table2)))))))
      (dotimes (i nitems (leave nitems))
        (let ((h1 (logand mask (random (ash 1 32) random-state)))
              (h2 (logand mask (random (ash 1 32) random-state))))
          (declare (optimize (safety 0)))
          (dotimes (j probe-depth (if (< stash-size max-stash)
                                      (incf stash-size)
                                      (leave i)))
            (let ((c1 (aref table1 (logand mask (+ h1 j))))
                  (c2 (aref table2 (logand mask (+ h2 j)))))
              (when (and (< c2 c1)
                         (< c2 bucket-size))
                (incf (aref depth j))
                (setf (aref table2 (+ h2 j))
                      (1+ c2))
                (return))
              (when (and (<= c1 c2)
                         (< c1 bucket-size))
                (incf (aref depth j))
                (setf (aref table1 (+ h1 j))
                      (1+ c1))
                (return)))))))))

(defun simulate-2-flip-addressing (size nitems &optional
                                   (probe-depth 2)
                                   (bucket-size 1)
                                   (max-stash 0)
                                   (random-state *random-state*))
  (declare (type (and unsigned-byte fixnum)
                 size nitems probe-depth bucket-size max-stash)
           (type random-state random-state)
           (optimize speed))
  (assert (= 2 probe-depth))
  (let ((table1 (get-ub32-vector size))
        (table2 (get-ub32-vector size))
        (depth  (get-ub64-vector probe-depth))
        (mask  (1- size))
        (stash-size 0))
    (declare (type (and fixnum unsigned-byte) stash-size))
    (flet ((leave (n)
             (declare (type (and unsigned-byte fixnum) n))
             (let ((load (round (* n 100) nitems)))
               (declare (ignorable load))
               (free-ub32-vector table1)
               (free-ub32-vector table2)
               (return-from simulate-2-flip-addressing
                 (list (make-singleton-histogram load)
                       (make-singleton-histogram stash-size)
                       depth
                       (merge-histograms (histogram table1)
                                         (histogram table2)))))))
      (dotimes (i nitems (leave nitems))
        (let ((h1 (logand mask (random (ash 1 32) random-state)))
              (h2 (logand mask (random (ash 1 32) random-state))))
          (declare (optimize (safety 0)))
          (dotimes (j probe-depth (if (< stash-size max-stash)
                                      (incf stash-size)
                                      (leave i)))
            (let ((c1 (aref table1 (logand mask (logxor h1 j))))
                  (c2 (aref table2 (logand mask (logxor h2 j)))))
              (when (and (< c2 c1)
                         (< c2 bucket-size))
                (incf (aref depth j))
                (setf (aref table2 (+ h2 j))
                      (1+ c2))
                (return))
              (when (and (<= c1 c2)
                         (< c1 bucket-size))
                (incf (aref depth j))
                (setf (aref table1 (+ h1 j))
                      (1+ c1))
                (return)))))))))

(defun make-n-random-states (n &optional (start (get-internal-run-time)))
  (map-into (make-array n)
            (lambda ()
              (sb-kernel::%make-random-state
               :state (sb-kernel::init-random-state
                       (logand (incf start)
                               #xffffffff))))))

(defparameter *nthreads* 8)

(defun run-n-simulations (simulator n merge &optional (nthreads *nthreads*))
  "That's pretty much a trivial parallel iterate/reduce"
  (let* ((histograms (make-array nthreads :initial-element nil))
         (count      0))
    (map nil #'sb-thread:join-thread
         (map 'vector
              (lambda (state)
                (prog1
                    (let ((count count))
                      (sb-thread:make-thread
                       (lambda ()
                         (let ((*random-state* state)
                               (*ub64-vectors* (list nil))
                               (*ub32-vectors* (list nil))
                               (*ub8-vectors* (list nil))
                               (histogram      nil))
                           (dotimes (i (if (= count (1- nthreads))
                                           (- n
                                              (* (1- nthreads)
                                                 (floor n nthreads)))
                                           (floor n nthreads))
                                     (setf (aref histograms count)
                                           histogram))
                             (let ((result (funcall simulator)))
                               (setf histogram
                                     (if histogram
                                         (funcall merge result histogram)
                                         result))))))))
                  (incf count)))
              (make-n-random-states nthreads)))
    (reduce (lambda (x y)
              (if (null x)
                  y
                  (funcall merge x y)))
            histograms)))
