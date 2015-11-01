(in-package "SB-VM")

(define-instruction cpuid (segment)
  #+nil(:printer two-bytes ((op '(#b00001111 #b10100010))))
  (:emitter
   (emit-byte segment #b00001111)
   (emit-byte segment #b10100010)))

(define-instruction rdtsc (segment)
  #+nil(:printer two-bytes ((op '(#b00001111 #b00110001))))
  (:emitter
   (emit-byte segment #b00001111)
   (emit-byte segment #b00110001)))

(defknown %read-cycle-counter () (values (unsigned-byte 32) (unsigned-byte 32)) ())

(define-vop (%read-cycle-counter)
  (:policy :fast-safe)
  (:translate %read-cycle-counter)
  (:temporary (:sc unsigned-reg :offset eax-offset :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target hi) edx)
  (:temporary (:sc unsigned-reg :offset ebx-offset) ebx)
  (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
  (:ignore ebx ecx)
  (:results (hi :scs (unsigned-reg))
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 5
     (inst xor eax eax)
     ;; Intel docs seem quite consistent on only using CPUID before RDTSC,
     ;; not both before and after. Go figure.
     (inst cpuid)
     (inst rdtsc)
     (move lo eax)
     (move hi edx)))

(defmacro with-cycle-counter (&body body)
  "Returns the primary value of BODY as the primary value, and the
number of CPU cycles elapsed as secondary value. EXPERIMENTAL."
  (with-unique-names (hi0 hi1 lo0 lo1)
    `(multiple-value-bind (,hi0 ,lo0) (%read-cycle-counter)
       (values (locally ,@body)
               (multiple-value-bind (,hi1 ,lo1) (%read-cycle-counter)
                 (+ (ash (- ,hi1 ,hi0) 32)
                    (- ,lo1 ,lo0)))))))
