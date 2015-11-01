;;;; Multiply/shift generator
;;;;
;;;; Computes (x*m)>>s without overflow, for m and s constant
;;;; and x a word.
;;;;
;;;; Returns a cost value, and the expression itself.
;;;; If no generator is available (e.g. s > 2 n-word-bits), returns nil.
(declaim (inline emit-trivial-mul-shift emit-mulhi-shift emit-slow-mul-shift))
(defun emit-trivial-mul-shift (max mul shift)
  "If everything fits in a word, life's simple"
  (when (and (typep mul 'sb-vm:word)
             (zerop (sb-kernel:%multiply-high max mul)))
    (list 1 `(ash (* x ,mul) ,(- shift)))))

(defun emit-mulhi-shift (max mul shift)
  "When shift >= word-size, and multiplier is a word,
 we can use a multiply-high."
  (declare (ignore max))
  (cond ((< shift sb-vm:n-word-bits) nil)
        ((typep mul 'sb-vm:word)
         (list 2
               (if (= shift sb-vm:n-word-bits)
                   `(sb-kernel:%multiply-high x ,mul)
                   `(ash (sb-kernel:%multiply-high x ,mul)
                         ,(- sb-vm:n-word-bits shift)))))
        ((<= (integer-length mul) (1+ sb-vm:n-word-bits))
         (list 3
               `(let ((high (sb-kernel:%multiply-high
                             x ,(ldb (byte sb-vm:n-word-bits 0) mul))))
                  ,(if (= shift sb-vm:n-word-bits)
                       `(+ x high)
                       `(ash (+ high (ash (- x high) -1))
                             ,(- 1 (- shift sb-vm:n-word-bits)))))))))

(defun emit-slow-mul-shift (max mul shift)
  "If we can let shift = 2*word-size, and mul fit in a double word as well,
emit this sequence."
  (let ((remaining-shift (- (* 2 sb-vm:n-word-bits) shift)))
    (when (<= (+ remaining-shift (integer-length max))
              sb-vm:n-word-bits)
      (list 6
            (let ((mulh (ash mul (- sb-vm:n-word-bits)))
                  (mull (ldb (byte sb-vm:n-word-bits 0) mul)))
              `(let* ((x   (ash x ,remaining-shift))
                      (low (sb-kernel:%multiply-high x ,mull)))
                 (values (sb-bignum:%multiply-and-add x ,mulh low))))))))

(defun maybe-emit-mul-shift (max mul shift &optional (errorp nil))
  "Sequentially try more expensive generators until one applies."
  (declare (type sb-vm:word max)
           (type unsigned-byte mul)
           (type (integer 0 #.(* 2 sb-vm:n-word-bits)) shift))
  (macrolet ((try (value)
               ` (let ((value ,value))
                   (when value
                     (return-from maybe-emit-mul-shift value)))))
    ;; are we lucky?
    (try (emit-trivial-mul-shift max mul shift))
    ;; try and increase shift to at least n-word-bits by only
    ;; scaling the multiplier
    (when (< shift sb-vm:n-word-bits)
      (let ((len (min (- sb-vm:n-word-bits (integer-length mul))
                      (- sb-vm:n-word-bits shift))))
        (when (> len 0)
          (setf mul    (ash mul len)
                shift  (+ shift len)))))
    (try (emit-mulhi-shift max mul shift))
    ;; again, increase shift, but frob the argument as well
    (let ((max     (- sb-vm:n-word-bits (integer-length max)))
          (unshift (- sb-vm:n-word-bits shift)))
      (when (<= unshift max)
        (let ((sequence (emit-mulhi-shift max mul (+ shift unshift))))
          (try (and sequence
                    (list (+ 2 (first sequence))
                          `(let ((x (ash x ,unshift)))
                             ,(second sequence))))))))
    ;; final case: get the shift amount to exactly 2*n-word-bits,
    ;; and emit the general case.
    (let ((reshift (min (- (* 2 sb-vm:n-word-bits) shift)
                        (- (* 2 sb-vm:n-word-bits) (integer-length mul)))))
      (when (plusp reshift)
        (setf shift (+ shift reshift)
              mul   (ash mul reshift))))
    (try (emit-slow-mul-shift max mul shift))
    (when errorp
      (error "Unable to emit multiply/shift sequence for (~S ~A ~A ~A)"
             'maybe-emit-mul-shift max mul shift))))


;;;; Truncate generator
;;;;
;;;; We usually use the over-approximation scheme, as it's very easy to
;;;; find good constants (see function below).
;;;;
;;;; However, when dividing by a constant (e.g. multiplying by 1/d), and
;;;; when X can be safely incremented by one, we try to use the under-
;;;; approximation scheme as well.
(declaim (ftype (function (sb-vm:word sb-vm:word sb-vm:word)
                          (values unsigned-byte (integer 0 #. (* 2 sb-vm:n-word-bits))
                                  &optional))
                find-mul-div-constants))
(defun find-over-approximation-constants (n m d)
  "Find the smallest constant mul and shift value s such that 
 [x*mul/2^s] = [x*m/d], for all x <= n"
  (declare (type sb-vm:word n m d))
  (let* ((max-shift (* 2 sb-vm:n-word-bits))
         (low       (truncate (ash m max-shift) d))
         (high      (truncate (+ (ash m max-shift)
                                 ;; could under-approximate with a power of two
                                 (1- (ceiling (ash 1 max-shift) n)))
                              d))
         (max-unshift (1- (integer-length (logxor low high))))
         (min-shift (- max-shift max-unshift)))
    (values (ash high (- max-unshift))
            min-shift)))

(declaim (inline find-under-approximation-constants))
(defun find-under-approximation-constants (n d)
  "Work in the under-approximatiom scheme: [x/d] = [(x+d)*mul/2^s]."
  (declare (type sb-vm:word n d))
  (when (>= n (1- (ash 1 sb-vm:n-word-bits)))
    (return-from find-under-approximation-constants nil))
  (let* ((shift (+ sb-vm:n-word-bits (1- (integer-length d))))
         (approx (truncate (ash 1 shift) d))
         (round  (round (ash 1 shift) d)))
    (assert (typep shift 'sb-vm:word))
    (when (= approx round)
      (values approx shift))))

(defun find-under-approximation-emitter (n d)
  (declare (type sb-vm:word n d))
  (multiple-value-bind (mul shift) (find-under-approximation-constants n d)
    (and mul shift
         (if (= shift sb-vm:n-word-bits)
             `(sb-kernel:%multiply-high (1+ x) ,mul)
             `(ash (sb-kernel:%multiply-high (1+ x) ,mul)
                   ,(- sb-vm:n-word-bits shift))))))

(defun emit-truncate-sequence-1 (max-n d)
  "Generate code for a truncated division.
First, check for powers of two (:
Then, go for the straightforward over-approximation sequence 
when it's cheap enough (an in-word multiply/shift or a multiply-high).
Otherwise, try to exploit an under-approximation, or to mask away
low bits.
When all of these fail, go for the generic over-approximation."
  (declare (type sb-vm:word max-n d))
  (when (zerop (logand d (1- d)))
    `(ash x ,(- (integer-length (1- d)))))
  (let ((vanilla (multiple-value-call #'maybe-emit-mul-shift max-n
                   (find-over-approximation-constants max-n 1 d) t))
        (gcd     (1- (integer-length (logand d (- d))))))
    (cond ((<= (first vanilla) 2)
           (second vanilla))
          ((find-under-approximation-emitter max-n d))
          ((plusp gcd)
           (let ((mask (1- (ash 1 gcd))))
             `(let ((x (logandc2 x ,mask)))
                ,(second (multiple-value-call #'maybe-emit-mul-shift
                           (logandc2 max-n mask)
                           (find-over-approximation-constants (ash max-n (- gcd))
                                                   1 (ash d (- gcd)))
                           t)))))
          (t
           (second vanilla)))))

(defun emit-truncate-sequence-2 (max-n m d)
  "Generate code for a truncated multiplication by a fraction < 1.
Go for the generic over-approximation scheme, except when we hit the full
2-word code sequence.
In that case, try to simplify the truncation by factorising the multiplier
in a simply \"perfect\" multiply-high sequence and a division."
  (declare (type sb-vm:word max-n m d))
  (assert (< m d))
  (let* ((vanilla (multiple-value-call #'maybe-emit-mul-shift max-n
                    (find-over-approximation-constants max-n m d) t))
         (gcd     (min (1- (integer-length (logand d (- d))))
                       (- sb-vm:n-word-bits (integer-length m))))
         (preshift (- sb-vm:n-word-bits gcd))
         (temp    (ash (* m max-n) (- gcd))))
    (if (or (< (first vanilla) 6)
            (zerop gcd)
            (> (+ (integer-length m) preshift) sb-vm:n-word-bits)
            (plusp (ash (sb-kernel:%multiply-high m max-n) (- gcd))))
        (second vanilla)
        `(let ((x (sb-kernel:%multiply-high x ,(ash m preshift))))
           ,(emit-truncate-sequence-1 temp (ash d (- gcd)))))))

(defun emit-truncate-sequence-3 (max-n m d)
  "Generate code for a truncated multiplication by a fraction > 1.
Go for the generic over-approximation if possible. Otherwise, split
the fraction in its integral and fractional parts, and emit a truncated
multiplication by a fraction < 1."
  (declare (type sb-vm:word max-n m d))
  (or (multiple-value-call #'maybe-emit-mul-shift max-n
        (find-over-approximation-constants max-n m d))
      (multiple-value-bind (q r) (truncate m d)
        `(+ (* x ,q)
            ,(if (= r 1)
                 (emit-truncate-sequence-1 max-n d)
                 (emit-truncate-sequence-2 max-n r d))))))

(defun emit-truncate-sequence (max-n m d)
  "Yeah (:"
  (declare (type sb-vm:word max-n m d))
  (cond ((= 1 d)
         `(* x ,m))
        ((= 1 m)
         (emit-truncate-sequence-1 max-n d))
        ((< m d)
         (emit-truncate-sequence-2 max-n m d))
        (t
         (emit-truncate-sequence-3 max-n m d))))
