(defpackage :vop-examples
  (:use :cl))

(in-package :sb-vm)

(defknown my-symbol-hash (symbol) (integer 0 #.most-positive-fixnum))
(define-vop (my-symbol-hash)
  (:policy :fast-safe)
  (:translate my-symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; cdr slot, so we have to strip off the three low bits to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst and res (lognot #b111))))
(defun vop-examples:my-symbol-hash (symbol)
  (my-symbol-hash symbol))

