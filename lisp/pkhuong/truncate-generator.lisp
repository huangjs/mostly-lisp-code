(in-package "SB-VM")
(define-vop (bignum-mult-c)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax))
  (:arg-types unsigned-num (:constant word))
  (:info y)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from :argument
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 19
    (move eax x)
    (cond ((typep y '(signed-byte 32))
           (inst mov edx y)
           (setf y edx))
          (t
           (setf y (register-inline-constant :qword y))))
    (inst mul eax y)
    (move hi edx)))

(define-vop (fixnum-mult-c)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (any-reg) :target eax))
  (:arg-types positive-fixnum (:constant word))
  (:info y)
  (:temporary (:sc any-reg :offset eax-offset :from (:argument 0)) eax)
  (:temporary (:sc any-reg :offset edx-offset :from :argument
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 14
    (move eax x)
    (cond ((typep y '(signed-byte 32))
           (inst mov edx y)
           (setf y edx))
          (t
           (setf y (register-inline-constant :qword y))))
    (inst mul eax y)
    (move hi edx)
    (inst and hi (lognot fixnum-tag-mask))))

(define-vop (bignum-mult-and-add-3-arg-c)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
         (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num (:constant word) unsigned-num)
  (:info y)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
                   :to :result) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from :argument
                   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 19
    (move eax x)
    (cond ((typep y '(signed-byte 32))
           (inst mov edx y)
           (setf y edx))
          (t
           (setf y (register-inline-constant :qword y))))
    (inst mul eax y)
    (inst add eax carry-in)
    (inst adc edx 0)
    (move hi edx)))
(in-package "CL-USER")

(defconstant +word-size+ sb-vm:n-word-bits)
(defconstant +max-word+ (1- (ash 1 +word-size+)))
(defvar *muldiv-data*)

(defstruct (muldiv-data
             (:constructor %make-muldiv-data
              (fraction multiplier divisor max-n known x cache)))
  fraction multiplier divisor max-n known x
  cache
  (under-cache nil) (over-cache nil))

(defun make-muldiv-data (fraction max-n x &key (known 1))
  (%make-muldiv-data fraction (numerator fraction) (denominator fraction)
                     max-n known x
                     (make-hash-table :test #'equal)))

(defun copy-muldiv-data-with (original &key
                              (multiplier (muldiv-data-multiplier original))
                              (divisor (muldiv-data-divisor original))
                              (max-n (muldiv-data-max-n original))
                              (known (muldiv-data-known original))
                              (x (muldiv-data-x original)))
  (%make-muldiv-data (/ multiplier divisor) multiplier divisor
                     max-n known x (muldiv-data-cache original)))

(defmacro with-muldiv-data ((fraction x max-n &key (known-divisor 1))
                            &body body)
  `(let ((*muldiv-data* (make-muldiv-data ,fraction ,max-n ,x :known ,known-divisor)))
     ,@body))

(defmacro with-copied-muldiv-data ((&rest keys
                                          &key multiplier
                                          divisor
                                          max-n
                                          known
                                          x)
                                   &body body)
  (declare (ignore multiplier divisor max-n known x))
  `(let ((*muldiv-data* (copy-muldiv-data-with
                         *muldiv-data*
                         ,@keys)))
     ,@body))

(defmacro with-muldiv-values ((&rest bindings) &body body)
  `(let* ((data     *muldiv-data*)
          (fraction (muldiv-data-fraction data))
          (divisor  (muldiv-data-divisor data))
          (multiplier (muldiv-data-multiplier data))
          (max      (muldiv-data-max-n data))
          (known    (muldiv-data-known data))
          (x        (muldiv-data-x data))
          ,@bindings)
     (declare (ignorable fraction divisor multiplier
                         max known x))
     ,@body))

;; Heart of the thing.
(defun approximation-ok-p (alpha shift &optional beta)
  (declare (type unsigned-byte alpha shift)
           (type (or unsigned-byte null) beta))
  (with-muldiv-values ((approx (/ alpha (ash 1 shift)))
                       (delta  (abs (- approx fraction)))
                       (gcd    (gcd divisor known)))
    (cond ((not beta)
           (assert (>= approx fraction))
           ;; max   < gcd/(delta divisor)
           ;; delta < gcd/(divisor max)
           (let ((max-delta (/ gcd (* divisor max))))
             (cond ((< delta max-delta))
                   (t
                    (loop for i upfrom (1+ shift) upto (* 2 +word-size+)
                          do (let ((inc (/ (ash 1 i))))
                               (decf delta inc)
                               (when (< delta max-delta)
                                 (return (values nil (1- i)))))
                          finally (return (values nil (* 2 +word-size+))))))))
          (t
           (assert (<= approx fraction))
           ;; max <= r/delta
           ;; delta <= r/max
           (let ((max-delta (/ beta (ash 1 shift) max))
                 (r (/ beta (ash 1 shift))))
             (assert (< r (/ gcd divisor)))
             (cond ((<= delta max-delta))
                   (t
                    (loop for i upfrom (1+ shift) upto (* 2 +word-size+)
                          do (let ((inc (/ (ash 1 i))))
                               (decf delta inc)
                               (when (<= delta max-delta)
                                 (return (values nil (1- i)))))
                          finally (return (values nil (* 2 +word-size+)))))))))))

(defun get-cache (under &aux (data *muldiv-data*))
  (let ((fast (if under
                  (muldiv-data-under-cache data)
                  (muldiv-data-over-cache data))))
    (when fast
      (return-from get-cache fast)))
  (let* ((data *muldiv-data*)
         (cache (muldiv-data-cache data))
         (key (list (muldiv-data-multiplier data)
                    (muldiv-data-divisor data)
                    (muldiv-data-max-n data)
                    (muldiv-data-known data)
                    under))
         (cur-cache (or (gethash key cache)
                        (setf (gethash key cache)
                              (make-array (1+ (* 2 +word-size+))
                                          :initial-element 0)))))
    (if under
        (setf (muldiv-data-under-cache data) cur-cache)
        (setf (muldiv-data-over-cache data) cur-cache))))

(defun approx (shift &key (under nil))
  (assert (<= shift (* 2 +word-size+)))
  (flet ((approx ()
           (with-muldiv-values ()
             (multiple-value-bind (alpha beta)
                 (if under
                     (values (floor (* (ash 1 shift) fraction))
                             (floor (/ (ash (gcd known divisor) shift) divisor)))
                     (values (ceiling (* (ash 1 shift) fraction)) nil))
               (multiple-value-bind (ok known-bad)
                   (approximation-ok-p alpha shift beta)
                 (if ok
                     (cons alpha beta)
                     (values nil (or known-bad shift))))))))
    (let* ((cache (get-cache under))
           (entry (aref cache shift)))
      (when (eql entry 0)
        (multiple-value-bind (approx known-bad)
            (approx)
          (setf (aref cache shift) approx)
          (setf entry approx)
          (unless entry
            (loop for i from (1+ shift) upto known-bad
                  do (setf (aref cache i) nil)))))
      (values (car entry) (cdr entry)))))

;; Inner loop.
(defun find-approx (&key
                    (min-shift 0) (max-shift (* 2 +word-size+))
                    (max-alpha (1- (ash 1 (* 2 +word-size+))))
                    (max-beta  nil))
  (cond ((not max-beta)
         (loop for i from min-shift upto max-shift
               for approx = (approx i)
               do
               (when approx
                 (return (and (<= approx max-alpha) (values i approx))))))
        ((and (eql t max-alpha)
              (/= 1 (muldiv-data-multiplier *muldiv-data*)))
         nil)
        (t
         (loop for i from min-shift upto max-shift
               do
            (multiple-value-bind (alpha beta) (approx i :under t)
              (when alpha
                (return (and (<= alpha max-alpha)
                             (case max-beta
                               ((t :inf) t)
                               (otherwise (<= beta max-beta)))
                             (values i alpha beta)))))))))

(declaim (inline wordp lb gcd-power-2))
(defun wordp (x)
  (<= x +max-word+))

(defun lb (x &optional under)
  (integer-length (if under (1- x) x)))

(defun gcd-power-2 (x)
  (let ((first (logand x (- x))))
    (integer-length (1- first))))

(defun generate-mask-sequence (generator &aux
                               (power (gcd-power-2
                                       (muldiv-data-divisor *muldiv-data*))))
  (unless (and (= (muldiv-data-multiplier *muldiv-data*) 1)
               (plusp power))
    (return-from generate-mask-sequence))
  (let ((mask (1- (ash 1 power))))
    (with-muldiv-values ()
      (with-copied-muldiv-data (:known (1+ mask)
                                :max-n (logandc2 max mask)
                                :x    `(logandc2 ,x ,mask))
        (funcall generator)))))

(defun generate-shift-sequence (generator &aux
                                (power (gcd-power-2
                                        (muldiv-data-divisor *muldiv-data*))))
  (unless (and (= (muldiv-data-multiplier *muldiv-data*) 1)
               (plusp power))
    (return-from generate-shift-sequence))
  (with-muldiv-values ()
    (with-copied-muldiv-data (:known   1
                              :divisor (ash divisor (- power))
                              :max-n   (ash max (- power))
                              :x      `(ash ,x ,(- power)))
      (funcall generator))))

;; Easy cases
(defun generate-level-0-sequence ()
  (with-muldiv-values (max-result)
    (cond ((= 1 divisor)
           `(* ,x ,multiplier))
          ((zerop (setf max-result (truncate (* max fraction))))
           0)
          ((and (= 1 multiplier)
                (zerop (logand divisor (1- divisor))))
           `(ash ,x ,(integer-length (1- divisor))))
          #+nil
          ((and (= 1 max-result)
                (> divisor 1))
           (let ((break (ceiling (/ fraction))))
             (when (wordp break)
               `(if (< ,x ,break) 0 1)))))))

;; Level 1: single-word arithmetic
(defun generate-within-word-multiply-shift ()
  (with-muldiv-values ()
    (multiple-value-bind (shift alpha)
        (find-approx :max-alpha (floor +max-word+ max))
      (when (and shift alpha)
        (assert (wordp (* max alpha)))
        `(ash (* ,x ,alpha) ,(- shift))))))

(defun generate-within-word-multiply-add-shift ()
  (with-muldiv-values ()
    (multiple-value-bind (shift alpha beta)
        (find-approx :max-alpha (floor (ash 1 +word-size+) max)
                     :max-beta :inf)
      (when (and shift alpha beta
                 (wordp (+ beta (* max alpha))))
        `(ash ,(if (= alpha beta)
                   `(* (1+ ,x) ,alpha)
                   `(+ (* ,x ,alpha) ,beta))
              ,(- shift))))))

(defun generate-level-1-sequence ()
  (or (generate-within-word-multiply-shift)
      (generate-within-word-multiply-add-shift)
      (generate-mask-sequence #'generate-within-word-multiply-shift)
      (generate-shift-sequence #'generate-within-word-multiply-shift)))

(defun generate-optimal-sequence ()
  (multiple-value-bind (shift alpha)
      (find-approx :max-alpha (1- (ash 1 +word-size+)))
    (unless (and shift alpha)
      (return-from generate-optimal-sequence))
    (when (< shift +word-size+)
      (let ((slack (min (- +word-size+ (lb alpha))
                        (- +word-size+ shift))))
        (setf alpha (ash alpha slack)
              shift (+ shift slack))
        (assert (wordp alpha))))
    (with-muldiv-values ((pre  (max 0 (- +word-size+ shift)))
                         (post (max 0 (- shift +word-size+))))
      (when (wordp (ash max pre))
        `(ash (sb-bignum:%multiply (ash ,x ,pre) ,alpha)
              ,(- post))))))

(defun generate-optimal-inc-sequence ()
  (unless (and (= 1 (muldiv-data-multiplier *muldiv-data*))
               (wordp (1+ (muldiv-data-max-n *muldiv-data*))))
    (return-from generate-optimal-inc-sequence))
  (multiple-value-bind (shift alpha)
      (find-approx :max-alpha (1- (ash 1 +word-size+)) :max-beta t)
    (unless (and shift alpha)
      (return-from generate-optimal-inc-sequence))
    (when (< shift +word-size+)
      (let ((slack (min (- +word-size+ (lb alpha))
                        (- +word-size+ shift))))
        (setf alpha (ash alpha slack)
              shift (+ shift slack))
        (assert (wordp alpha))))
    (with-muldiv-values ((pre  (max 0 (- +word-size+ shift)))
                         (post (max 0 (- shift +word-size+))))
      (when (wordp (ash max pre))
        `(ash (sb-bignum:%multiply (ash (1+ ,x) ,pre) 
                                   ,alpha)
              ,(- post))))))

(defun generate-level-2-sequence ()
  (or (generate-optimal-sequence)
      (generate-optimal-inc-sequence)
      (generate-mask-sequence #'generate-optimal-sequence)
      (generate-shift-sequence #'generate-optimal-sequence)))

(defun generate-implicit-1-sequence ()
  (multiple-value-bind (shift alpha)
      (find-approx :min-shift +word-size+)
    (unless (and shift alpha)
      (return-from generate-implicit-1-sequence))
    (with-muldiv-values ((hi (ash alpha (- +word-size+)))
                         (lo (ldb (byte +word-size+ 0) alpha))
                         (l  (- shift +word-size+)))
      (unless (wordp (* max hi))
        (return-from generate-implicit-1-sequence))
      (if (or (zerop l)
              (wordp (+ (* hi max)
                        (ash (* lo max) (- +word-size+)))))
          `(let* ((x  ,x)
                  (n  `(* x ,hi))
                  (t1 (sb-bignum:%multiply x ,lo)))
             `(ash (+ t1 n) ,(- l)))
          `(let* ((x  ,x)
                  (n  (* x ,hi))
                  (t1 (sb-bignum:%multiply x ,lo))
                  (t2 (ash (- n t1) -1)))
             (ash (+ t1 t2) ,(- 1 l)))))))

(defun generate-mul-mul-shift-sequence ()
  (with-muldiv-values ((max-shift (min (gcd-power-2 divisor)
                                       (- +word-size+ (lb multiplier)))))
    (unless (and (wordp (ash multiplier (- +word-size+ max-shift)))
                 (wordp (ash (* max multiplier) (- max-shift))))
      (return-from generate-mul-mul-shift-sequence))
    (with-copied-muldiv-data (:x `(sb-bignum:%multiply
                                   ,x
                                   ,(ash multiplier (- +word-size+ max-shift)))
                              :known 1
                              :multiplier 1
                              :divisor (ash divisor (- max-shift))
                              :max-n (ash (* max multiplier) (- max-shift)))
      (generate-optimal-sequence))))

(defun generate-level-3-sequence ()
  (or (generate-implicit-1-sequence)
      (generate-mask-sequence #'generate-implicit-1-sequence)
      (generate-shift-sequence #'generate-implicit-1-sequence)
      (generate-mul-mul-shift-sequence)
      (generate-shift-sequence #'generate-mul-mul-shift-sequence)))

(defun generate-general-case ()
  (let ((alpha (approx (* 2 +word-size+))))
    (unless (< alpha (ash 1 (* 2 +word-size+)))
      (return-from generate-general-case))
    (let ((lo (ldb (byte +word-size+ 0) alpha))
          (hi (ash alpha (- +word-size+))))
      (assert (wordp hi))
      `(let* ((x ,(muldiv-data-x *muldiv-data*))
              (lo (sb-bignum:%multiply x ,lo)))
         (sb-bignum:%multiply-and-add x ,hi lo)))))

(defun generate-level-4-sequence ()
  (or (generate-general-case)
      (generate-shift-sequence #'generate-general-case)))

(defun %decompose-multiplier (generator)
  (unless (> (muldiv-data-fraction *muldiv-data*) 1)
    (return-from %decompose-multiplier))
  (or (with-muldiv-values ((integer (truncate fraction)))
        (or (let* ((pow2 (1- (integer-length integer)))
                   (mul  (ash 1 pow2))
                   (body (with-copied-muldiv-data
                             (:x 'x
                              :multiplier (- multiplier (* mul divisor)))
                           (funcall generator))))
              (when (and body (wordp (ash max pow2)))
                `(let ((x ,x))
                   (+ ,body (ash x ,pow2)))))
            (when (wordp (* max integer))
              (let ((body (with-copied-muldiv-data
                              (:x 'x
                               :multiplier (- multiplier
                                              (* integer divisor)))
                            (funcall generator))))
                (when body
                  `(let ((x ,x))
                     (+ ,body (* x ,integer))))))))))

(defun decompose-multiplier (generator)
  (or (funcall generator)
      (%decompose-multiplier generator)))

(defun %generate ()
  (when (wordp (truncate (* (muldiv-data-max-n *muldiv-data*)
                            (muldiv-data-fraction *muldiv-data*))))
    (some #'decompose-multiplier '(generate-level-0-sequence
                                   generate-level-1-sequence
                                   generate-level-2-sequence
                                   generate-level-3-sequence
                                   generate-level-4-sequence))))

(defun peephole (tree)
  (flet ((default ()
             (mapcar #'peephole tree)))
    (etypecase tree
      (atom tree)
      ((cons (eql let))
         (destructuring-bind (let (&rest bindings) body) tree
           (declare (ignore let))
           (if (equal bindings '((x x)))
               (peephole body)
               (default))))
      ((cons (eql *))
         (destructuring-bind (_ a b) tree
           (declare (ignore _))
           (case b
             (0 0)
             (1 (peephole a))
             (t (if (and (integerp b)
                         (zerop (logand b (1- b))))
                    `(ash ,(peephole a) ,(integer-length (1- b)))
                    (default))))))
      ((cons (eql ash))
         (destructuring-bind (ash x shift) tree
           (declare (ignore ash))
           (case shift
             (0 (peephole x))
             (1 (let ((x (peephole x)))
                  (if (eql x 'x)
                      `(+ x x)
                      `(let ((x ,x))
                         (+ x x)))))
             (t (default)))))
      (t (default)))))

(defun generate-%unary-truncate (max-n divisor &key (known-divisor 1))
  (let ((max-result (truncate max-n divisor)))
    (if (= max-result 0)
        `(* x 0)
        (with-muldiv-data ((/ divisor) 'x max-n
                           :known-divisor known-divisor)
          (let ((body (peephole (%generate))))
            (and body
                 `(values (sb-ext:truly-the (integer 0 ,max-result)
                                            ,body))))))))
