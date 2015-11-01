;;; Basic sparse BFGS method for convex doubly differentiable function
;;;
;;;  ** Very early in development, do not use **
;;;
;;; Rationale is that the objective function will be a sum of log barriers
;;; on sparse linear functions.  As the multiplier approaches zero, only
;;; very few such barriers will be active enough that their gradient won't
;;; be nearly zero.  

(defstruct (sparse-vector
             (:constructor
              make-sparse-vector (length val idx))
             (:conc-name #:sv-))
  (length  0 :type (and unsigned-byte fixnum))
  (val   nil :type (simple-array double-float 1) :read-only t)
  (idx   nil :type (simple-array (unsigned-byte 32) 1) :read-only t))
(declaim (freeze-type sparse-vector))

(defglobal **default-sparse-vector**
    (make-sparse-vector 0
                        (make-array 0 :element-type 'double-float)
                        (make-array 0 :element-type '(unsigned-byte 32))))

(define-symbol-macro %empty-sparse-vector% (load-time-value **default-sparse-vector** t))

(defconstant +eps+ 1d-8)

(defun sparsify-vector (dense &optional (eps +eps+))
  (declare (type (simple-array double-float 1) dense)
           (type double-float eps))
  (let* ((length  (length dense))
         (count   (count-if-not (lambda (x)
                                  (declare (double-float x))
                                  (<= (abs x) eps))
                                dense))
         (values  (make-array count :element-type 'double-float))
         (indices (make-array count :element-type '(unsigned-byte 32)))
         (alloc   0))
    (dotimes (i length (prog1 (make-sparse-vector count values indices)
                         (assert (= alloc count))))
      (let ((x (aref dense i)))
        (unless (<= (abs x) eps)
          (setf (aref values  alloc) x
                (aref indices alloc) i
                alloc                (1+ alloc)))))))

;; generalise to a do-transposed-vector
(defmacro do-sparse-vector ((vector (&optional val idx length i)
                                    &optional result)
                            &body body)
  (let ((_vector (gensym "VECTOR"))
        (_length (gensym "LENGTH"))
        (_vals   (gensym "VALUES"))
        (_idxs   (gensym "INDICES"))
        (_i      (gensym "I")))
    `(let* ((,_vector ,vector)
            (,_length (sv-length ,_vector))
            (,_vals   (sv-val ,_vector))
            (,_idxs   (sv-idx ,_vector))
            ,@(and length
                `((,length ,_length))))
       (declare (type sparse-vector ,_vector))
       (dotimes (,_i ,_length ,result)
         (let (,@(and val
                   `((,val (aref ,_vals ,_i))))
               ,@(and idx
                   `((,idx (aref ,_idxs ,_i))))
               ,@(and i
                   `((,i ,_i))))
           ,@body)))))

;; optimise later. GPGPU or LRB <3
(defun dotprod (sparse dense)
  (declare (sparse-vector sparse)
           ((simple-array double-float 1) dense))
  (let ((sum 0d0))
    (do-sparse-vector (sparse (val idx) sum)
      (incf sum (* val (aref dense idx))))))

(defun axpy (a sparse dense)
  (declare (double-float a)
           (sparse-vector sparse)
           ((simple-array double-float 1) dense))
  (do-sparse-vector (sparse (val idx) dense)
    (incf (aref dense idx) (* a val))))

(defun scale-vector (scale dense)
  (declare (double-float scale)
           ((simple-array double-float 1) dense))
  (map-into dense (lambda (x)
                    (* scale x))
            dense))

(defun nresparsify-vector (sparse &optional (eps +eps+))
  (declare (sparse-vector sparse)
           (double-float eps))
  (let ((vals   (sv-val sparse))
        (idxs   (sv-idx sparse))
        (alloc  0))
    (dotimes (i (sv-length sparse) (progn
                                     (setf (sv-length sparse) alloc)
                                     sparse))
      (let ((val (aref vals i))
            (idx (aref idxs i)))
        (unless (<= (abs val) eps)
          (setf (aref vals alloc) val
                (aref idxs alloc) idx
                alloc             (1+ alloc)))))))

(defun scale-sparse-vector (scale sparse)
  (declare (double-float scale)
           (sparse-vector sparse))
  (scale-vector scale (sv-val sparse))
  sparse)

;; Represents a set of rank-1 corrections,
;; \sum scales_i * rows_i . rows_i^T
;; Assumes the initial matrix is the identity
(defstruct (corrections
             (:constructor %make-corrections (length rows columns scales)))
  (length   0 :type (and unsigned-byte fixnum))
  (rows    nil :type (simple-array sparse-vector 1))
  (columns nil :type (simple-array sparse-vector 1))
  (scales  nil :type (simple-array double-float 1)))
(declaim (freeze-type corrections))

(defun push-correction (row column scale corrections)
  (declare (type sparse-vector row)
           (type sparse-vector column)
           (type double-float scale)
           (type corrections corrections))
  (let ((length  (corrections-length  corrections))
        (rows    (corrections-rows    corrections))
        (columns (corrections-columns corrections))
        (scales  (corrections-scales  corrections)))
    (when (or (zerop (sv-length row))
              (zerop (sv-length column)))
      (return-from push-correction corrections))
    (when (>= length (length rows))
      (setf rows   (replace (make-array (* 2 length)
                                        :element-type 'sparse-vector
                                        :initial-element %empty-sparse-vector%)
                            rows)
            columns (replace (make-array (* 2 length)
                                         :element-type 'sparse-vector
                                         :initial-element %empty-sparse-vector%)
                             columns)
            scales (replace (make-array (* 2 length)
                                        :element-type 'double-float)
                            scales)
            (corrections-rows    corrections) rows
            (corrections-columns corrections) columns
            (corrections-scales  corrections) scales))
    (setf (aref rows    length) row
          (aref columns length) column
          (aref scales  length) scale
          (corrections-length corrections) (1+ length)))
  corrections)

(defun apply-corrections (corrections vector)
  (declare (type corrections corrections)
           (type (simple-array double-float 1) vector))
  ;; release this in the caller
  (let ((output (make-array (length vector)
                            :element-type 'double-float
                            :initial-contents vector))
        (length  (corrections-length  corrections))
        (rows    (corrections-rows    corrections))
        (columns (corrections-columns corrections))
        (scales  (corrections-scales  corrections)))
    (dotimes (i length output)
      (let ((row    (aref rows    i))
            (column (aref columns i))
            (scale  (aref scales  i)))
        (axpy (* scale (dotprod row vector))
              column
              output)))))

(defstruct (bfgs-state
             (:constructor
              %make-bfgs-state (corrections
                                position position-tmp value
                                gradient old-grad
                                eval)))
  (corrections nil :type corrections)
  (position    nil :type (simple-array double-float 1))
  ;; just a temporary copy of position
  (position-tmp nil :type (simple-array double-float 1))
  (value       0d0 :type double-float)
  (gradient    nil :type (simple-array double-float 1))
  (old-grad    nil :type (simple-array double-float 1))
  (eval        nil :type (function ((simple-array double-float 1)
                                    (simple-array double-float 1))
                                   (values double-float &optional))))

(defun make-bfgs-state (n eval &optional initial-position)
  (let* ((position (make-array n :element-type 'double-float))
         (gradient (make-array n :element-type 'double-float))
         (value    (progn
                     (when initial-position
                       (replace position initial-position))
                     (funcall eval position gradient))))
    (%make-bfgs-state
     (%make-corrections 0
                        (make-array 16
                                    :element-type   'sparse-vector
                                    :initial-element %empty-sparse-vector%)
                        (make-array 16
                                    :element-type   'sparse-vector
                                    :initial-element %empty-sparse-vector%)
                        (make-array 16 :element-type 'double-float))
     position (make-array n :element-type 'double-float) value
     gradient (make-array n :element-type 'double-float)
     eval)))

(defun line-search (state sparse-direction &optional (max-step double-float-positive-infinity))
  (declare (bfgs-state state)
           (sparse-vector sparse-direction)
           (double-float max-step))
  "Weighted (superlinear) false position.

   Note that the direction is an *ascent* direction"
  (let* ((fun        (bfgs-state-eval state))
         (old-position (bfgs-state-position state))
         (position   (bfgs-state-position-tmp state))
         (gradient   (bfgs-state-old-grad state)))
    (rotatef (bfgs-state-old-grad state) (bfgs-state-gradient state))
    (labels ((eval-step (step)
               (declare (double-float step))
               (replace position old-position)
               (axpy step sparse-direction position)
               (let ((value (funcall fun position gradient)))
                 (setf (bfgs-state-value state) value)
                 (if (= double-float-positive-infinity value)
                     double-float-negative-infinity
                     (dotprod sparse-direction gradient))))
             (find-max-step ()
               (let ((min-step 0d0)
                     (min-dz   (dotprod sparse-direction (bfgs-state-old-grad state)))
                     (max-step -1d0))
                 (loop
                   (let ((dz (eval-step max-step)))
                     (cond ((= dz double-float-positive-infinity)
                            (setf max-step (/ (+ min-step max-step) 2)))
                           ((< dz (- +eps+))
                            (return (values min-step min-dz
                                            max-step     dz)))
                           ((< dz +eps+)
                            (return-from line-search max-step))
                           (t
                            (setf min-step max-step
                                  min-dz   dz
                                  max-step (+ max-step (- max-step min-step))))))))))
      ;; first, bracket the zero
      (multiple-value-bind (min-step min-dz
                            max-step max-dz)
          (if (= max-step double-float-positive-infinity)
              (find-max-step)
              (values 0d0      (dotprod sparse-direction (bfgs-state-old-grad state))
                      max-step (eval-step max-step)))
        (let ((min-gamma 1d0)
              (max-gamma 1d0)
              (last-endpoint nil))
          (loop
            (let* ((weighted-min-dz (* min-gamma min-dz))
                   (weighted-max-dz (* max-gamma max-dz))
                   (midpoint (/ (- (* weighted-min-dz min-step) (* weighted-max-dz max-step))
                                (- weighted-min-dz weighted-max-dz)))
                   (mid-dz   (eval-step midpoint)))
              (cond ((or (< (abs (- min-step max-step)) +eps+)
                         (< min-gamma +eps+)
                         (< max-gamma +eps+))
                     (replace old-position position)
                     (return midpoint))
                    ((> mid-dz +eps+)
                     (setf max-gamma 1d0)
                     (setf min-gamma
                           (if (eq last-endpoint 'min)
                               (/ min-gamma 2d0)
                               1d0))
                     (setf last-endpoint 'min
                           min-step       midpoint
                           min-dz         mid-dz))
                    ((< mid-dz (- +eps+))
                     (setf min-gamma 1d0)
                     (setf max-gamma
                           (if (eq last-endpoint 'max)
                               (/ max-gamma 2d0)
                               1d0))
                     (setf last-endpoint 'max
                           max-step       midpoint
                           max-dz         mid-dz))
                    (t
                     (replace old-position position)
                     (return midpoint))))))))))

(defun add-correction (state sparse-direction step)
  (declare (type bfgs-state state)
           (type sparse-vector sparse-direction)
           (type double-float step))  
  (let* ((corrections     (bfgs-state-corrections state))
         (s               (nresparsify-vector
                           (scale-sparse-vector step sparse-direction)))
         (y               (map-into (bfgs-state-old-grad state)
                                    #'-
                                    (bfgs-state-gradient state)
                                    (bfgs-state-old-grad state)))
         (sparse-y        (sparsify-vector y))
         (|s^T y|         (dotprod s y))
         (|B^-1 y|        (apply-corrections corrections y))
         (|y^T B^-1 y|    (dotprod sparse-y |B^-1 y|))
         (|sparse B^-1 y| (sparsify-vector |B^-1 y|)))
    (push-correction s s (/ (+ |s^T y| |y^T B^-1 y|)
                            (expt |s^T y| 2))
                     corrections)
    (let ((scale (/ -1d0 |s^T y|)))
      (push-correction s |sparse B^-1 y| scale corrections)
      (push-correction |sparse B^-1 y| s scale corrections)))
  state)

(defun bfgs-iteration (state)
  (declare (type bfgs-state state))
  ;; check that the corrected direction is still an ascent direction (d^Tg > 0)
  (let* ((direction (sparsify-vector
                     (apply-corrections (bfgs-state-corrections state)
                                        (bfgs-state-gradient    state))))
         (step      (line-search state direction)))
    (add-correction state direction step)
    state))
