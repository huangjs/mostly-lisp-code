(in-package "SB-VM")

(defknown foo ((unsigned-byte 32) (unsigned-byte 32))
    (unsigned-byte 32))

(define-vop (foo)
  (:args (x :scs (unsigned-reg) :target res)
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate foo)
  (:policy :fast-safe)
  (:generator 1
    (move res x)
    (inst xor res y)))

(defun test (x y)
  (declare (type (unsigned-byte 32) x y))
  (foo x y))
