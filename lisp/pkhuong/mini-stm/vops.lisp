;;; # Low-level operations not (all) directly provided in SBCL
;;;
;;; opaque-identity is useful to foil static analyses, without affecting
;;; the final binary.  Mostly, it's used to work around the fact that
;;; load-time-value indirect through value-cell for non-read-only values.
;;; This way, I can fake mutable load-time-value by wrapping a read-only
;;; l-t-v with opaque-identity...
;;;
;;; cas-byte-sap performs a compare-and-swap on the byte pointed by a
;;; system-area-pointer.
;;;
;;; xadd-word-sap performs a (q)word-sized exchange and add on the
;;; address pointed by a system-area-pointer.
;;;
;;; membar provides a memory fence.

(in-package "SB-VM")
(defknown stm::opaque-identity (t) t)
(defknown stm::cas-byte-sap
    (system-area-pointer (unsigned-byte 8) (unsigned-byte 8))
    (unsigned-byte 8))
(defknown stm::xadd-word-sap (system-area-pointer word) word)
(defknown stm::membar () (values))
(define-vop (stm::opaque-identity)
    (:translate stm::opaque-identity)
    (:policy :fast-safe)
    (:args (x :scs (any-reg descriptor-reg) :target res))
    (:results (res :scs (any-reg descriptor-reg)))
    (:generator 0
       (move res x)))

(define-vop (stm::cas-byte-sap)
  (:translate stm::cas-byte-sap)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to :eval)
         (old :scs (unsigned-reg) :target rax)
         (new :scs (unsigned-reg)))
  (:temporary (:sc descriptor-reg :offset rax-offset
                   :from (:argument 1) :to :result :target result)
              rax)
  (:results (result :scs (unsigned-reg)))
  (:arg-types system-area-pointer unsigned-num unsigned-num)
  (:result-types unsigned-num)
  (:generator 5
     (move rax old)
     (inst cmpxchg (make-ea :byte :base sap)
           (make-byte-tn new) :lock)
     (inst movzx result al-tn)))

(define-vop (stm::xadd-word-sap)
  (:translate stm::xadd-word-sap)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to :eval)
         (inc :scs (unsigned-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:arg-types system-area-pointer unsigned-num)
  (:result-types unsigned-num)
  (:generator 5
     (inst xadd (make-ea :qword :base sap)
           inc :lock)
     (move result inc)))

(define-vop (stm::membar)
  (:translate stm::membar)
  (:policy :fast-safe)
  (:args)
  (:results)
  (:generator 10
    (inst mfence)))

(in-package "STM")
(defun opaque-identity (x)
  (opaque-identity x))

(defun cas-byte-sap (sap old new)
  (declare (type system-area-pointer sap)
           (type (unsigned-byte 8) old new))
  (cas-byte-sap sap old new))

(defun xadd-word-sap (sap word)
  (declare (type system-area-pointer sap)
           (type sb-vm:word word))
  (xadd-word-sap sap word))

(defun membar ()
  (membar)
  (values))
