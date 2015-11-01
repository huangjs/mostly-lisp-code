(defpackage "SPECIALS-INTROSPECT"
  (:use "CL" "SB-VM" "SB-SYS" "SB-KERNEL")
  (:export #:global-binding-p
           ;; symbol-FOO-value instead?
           #:thread-local-symbol-value #:global-symbol-value))

(in-package "SB-VM")
;; see cell.lisp:symbol-value
#+sb-thread
(define-vop (specials-introspect::tls-ref)
    (:args (index :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg)))
  #+x86-64
  (:generator 5
	      (inst mov value (make-ea :qword
				       :base thread-base-tn
				       :index index :scale 1)))
  #+x86
  (:generator 5
	      (inst fs-segment-prefix)
	      (inst mov value (make-ea :dword :base index))))

#+sb-thread
(define-vop (specials-introspect::tls-set)
    (:args (value :scs (descriptor-reg))
	   (index :scs (descriptor-reg)))
  (:results)
  #+x86-64
  (:generator 5
	      (inst mov (make-ea :qword
				 :base thread-base-tn
				 :index index :scale 1)
		    value))
  #+x86
  (:generator 5
	      (inst fs-segment-prefix)
	      (inst mov (make-ea :dword :base index) value)))

(define-vop (specials-introspect::%set-symbol-global-value)
    (:args (value  :scs (descriptor-reg))
	   (symbol :scs (descriptor-reg)))
  (:results)
  #+(or x86-64 x86)
  (:generator 5
	      (storew value symbol symbol-value-slot other-pointer-lowtag)))
(in-package "SPECIALS-INTROSPECT")

#+sb-thread
(defun global-binding-p (symbol)
  "Simply check that the symbol has no tls index,
   or that the tls slot is empty."
  (declare (type symbol symbol))
  (let ((index (sb-vm::symbol-tls-index symbol)))
    (or (zerop index)
        (eq (%primitive tls-ref index)
            (%make-lisp-obj no-tls-value-marker-widetag)))))

#-sb-thread
(defun global-binding-p (symbol)
  "Walk the binding stack to find out whether binding info
   was saved for [symbol]."
  (declare (type symbol symbol))
  (let* ((binding-stack-start (sb-vm::current-thread-offset-sap
                               sb-vm::thread-binding-stack-start-slot))
         (length (sap- (sb-vm::current-thread-offset-sap
                        sb-vm::thread-binding-stack-pointer-slot)
                       binding-stack-start)))
    (sb-sys:with-pinned-objects (symbol)
      (loop with word = (get-lisp-obj-address symbol)
	 for offset from 0 below length by sb-vm::binding-size
	 when (= word (sap-ref-word binding-stack-start
				    (+ offset (* sb-vm::binding-symbol-slot
						 n-word-bytes))))
	 do (return nil)
	 finally (return t)))))

#+sb-thread
(defun ensure-tls-index (symbol)
  (declare (type symbol symbol))
  (let ((index (sb-vm::symbol-tls-index symbol)))
    (unless (zerop index)
      (return-from ensure-tls-index index)))
  ;; HACK make sure an index gets allocated.
  (progv (list symbol) (list nil)
    (sb-vm::symbol-tls-index symbol)))

(defun thread-local-symbol-value (symbol)
  (declare (type symbol symbol))
  #+sb-thread
  (let ((value (%primitive tls-ref (ensure-tls-index symbol))))
    (if (eq value (%make-lisp-obj no-tls-value-marker-widetag))
        (values (symbol-value symbol) nil)
        (values value t)))
  #-sb-thread
  (values (symbol-value symbol) t))

(defun (setf thread-local-symbol-value) (value symbol)
  #+sb-thread
  (prog1 value
    (%primitive tls-set value (ensure-tls-index symbol)))
  #-sb-thread
  (setf (symbol-value symbol) value))

(defun global-symbol-value (symbol)
  (declare (type symbol symbol))
  (sb-vm::symbol-global-value symbol))

(defun (setf global-symbol-value) (value symbol)
  (declare (type symbol symbol))
  #+sb-thread
  (prog1 value
    (%primitive %set-symbol-global-value value symbol))
  #-sb-thread
  (setf (symbol-value symbol) value))
