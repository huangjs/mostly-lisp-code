;;;
;;; single-stepper.lisp
;;;
;;; Proof-of-concept instruction single-stepper for SBCL on x86/Linux.
;;;

(cl:defpackage :single-step (:use :common-lisp :sb-alien))

(in-package :single-step)

;;; This is a proof-of-concept, which means that it demonstrates
;;; the basic principle, but isn't necessarily ready for
;;; production use.  Basic operation is simple: Load it, hit
;;; C-c, and use the "step" restart. Keep using the "step"
;;; restart to step additional instructions, use the "continue"
;;; restart to resume without stepping, or use any other restart
;;; to resume from there without stepping.

(defconstant trace-flag #x100)

(defun sigint-handler (signal info context)
  (declare (ignore signal info))
  (sb-unix::with-interrupts
    (restart-case
	(sb-unix::sigint-%break "interrupted at #X~X"
		       (with-alien ((context (* sb-vm::os-context-t) context))
			 (sb-sys:sap-int (sb-vm::context-pc context))))
      (step ()
	:report "Step one instruction"
	(with-alien ((context (* sb-vm::os-context-t) context))
	  (let* ((eflags-address
		  (alien-funcall (extern-alien "context_eflags_addr"
					       (function unsigned-int
							 (* sb-vm::os-context-t)))
				 context))
		 (eflags-sap (sb-sys:int-sap eflags-address)))
	    ;; Just set the trace flag, the hardware will do the rest.
	    (setf (sb-sys:sap-ref-32 eflags-sap 0)
		  (logior trace-flag (sb-sys:sap-ref-32 eflags-sap 0)))))))))

(defun sigtrap-handler (signal info context)
  (declare (ignore signal info))
  (sb-unix::with-interrupts
    (with-alien ((context (* sb-vm::os-context-t) context))
      (let* ((eflags-address
	      (alien-funcall (extern-alien "context_eflags_addr"
					   (function unsigned-int
						     (* sb-vm::os-context-t)))
			     context))
	     (eflags-sap (sb-sys:int-sap eflags-address)))
	;; disable trace flag in case user uses CONTINUE restart.
	(setf (sb-sys:sap-ref-32 eflags-sap 0)
	      (logandc1 trace-flag (sb-sys:sap-ref-32 eflags-sap 0)))
	(restart-case
	    (sb-unix::sigint-%break "single-step trap at #X~X"
			   (with-alien ((context (* sb-vm::os-context-t) context))
			     (sb-sys:sap-int (sb-vm::context-pc context))))
	  (step ()
	    :report "Step one instruction"
	    ;; Re-set the trace flag.
	    (setf (sb-sys:sap-ref-32 eflags-sap 0)
		  (logior trace-flag (sb-sys:sap-ref-32 eflags-sap 0)))))))))

(sb-unix::enable-interrupt sb-unix:sigint #'sigint-handler)
(sb-unix::enable-interrupt sb-unix:sigtrap #'sigtrap-handler)

;;; EOF
