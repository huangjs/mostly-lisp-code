(in-package "SB!C")
(defun live-tns (vop)
  (let ((read-scs       (make-hash-table :test #'equal))
        (registered-scs (make-hash-table :test #'equal)))
    (labels ((register-read (tn)
               (let* ((sb (sc-sb (tn-sc tn)))
                      (offset (tn-offset tn))
                      (key (cons sb offset)))
                 (unless (gethash key registered-scs)
                   (setf (gethash key read-scs) tn
                         (gethash key registered-scs) tn))))
             (register-write (tn)
               (let* ((sb (sc-sb (tn-sc tn)))
                      (offset (tn-offset tn))
                      (key (cons sb offset)))
                 (unless (gethash key registered-scs)
                   (setf (gethash key registered-scs) tn))))
             (register-ref (tn-ref)
               (if (tn-ref-write-p tn-ref)
                   (register-write (tn-ref-tn tn-ref))
                   (register-read  (tn-ref-tn tn-ref))))
             (register-live-in ()
               (let ((2block (vop-block vop)))
                 (map nil (lambda (livep ltn)
                            (unless (zerop livep)
                              (register-read ltn)))
                      (ir2-block-live-in 2block)
                      (ir2-block-local-tns 2block))))
             (register-live ()
               (let ((2block (vop-block vop)))
                 (do ((conflict (ir2-block-global-tns 2block)
                                (global-conflicts-next-blockwise conflict)))
                     ((null conflict))
                   (when (eq (global-conflicts-kind conflict) :live)
                     (register-read (global-conflicts-tn conflict)))))))
      (do ((vop (vop-next vop)
                (vop-next vop)))
          ((null vop)
           (register-live-in)
           (register-live)
           (let (tns)
             (maphash (lambda (sc tn)
                        (declare (ignore sc))
                        (push tn tns))
                      read-scs)
             (nreverse tns)))
        (do ((read (vop-args vop)
                   (tn-ref-across read)))
            ((null read))
          (register-ref read))
        (do ((write (vop-results vop)
                    (tn-ref-across write)))
            ((null write))
          (register-ref write))))))
(in-package "SB!VM")

(defparameter *boxed-scs* '(control-stack any-reg descriptor-reg))
(defparameter *constant-scs* '(constant fp-single-zero fp-double-zero immediate))

(defknown register-locals ((simple-array t (4))) (values boolean &optional))

(defknown %call-locals ((simple-array t (4))) *)

(define-vop (register-locals)
  (:policy :fast-safe)
  (:vop-var  vop)
  (:node-var node)
  (:args    (vectors :scs (any-reg descriptor-reg)))
  (:temporary (:sc descriptor-reg) unboxed-vec)
  (:temporary (:sc descriptor-reg) boxed-vec)
  (:temporary (:sc unsigned-reg) tmp)
  (:conditional)
  (:translate register-locals)
  (:info target not-p)
  (:generator 50
    (let* ((2physenv (sb-c::physenv-info
                      (sb-c::lambda-physenv
                       (sb-c::node-home-lambda
                        node))))
           (old-fp (sb-c::ir2-physenv-old-fp 2physenv))
           (return-pc (sb-c::ir2-physenv-return-pc 2physenv))
           (live  (remove-if (lambda (tn)
                               (or (location= tn vectors)
                                   (location= tn old-fp)
                                   (location= tn return-pc)
                                   (memq (sb-c::sc-name (sb-c::tn-sc tn))
                                         *constant-scs*)))
                             (sb-c::live-tns vop)))
           (boxed (remove-if-not (lambda (tn)
                                   (memq (sb-c::sc-name (sb-c::tn-sc tn))
                                         *boxed-scs*))
                                 live))
           (unboxed (set-difference live boxed))
           (END     (gen-label))
           (RESTORE (gen-label))
           (capture-target (if not-p END target))
           (restore-target (if not-p target END))
           (ALLOC-UNBOXED   (gen-label))
           (UNBOXED-ALLOCED (gen-label))
           (ALLOC-BOXED     (gen-label))
           (BOXED-ALLOCED   (gen-label)))
      (inst lea tmp (make-fixup nil
                                :code-object RESTORE))
      (storew tmp vectors
              vector-data-offset other-pointer-lowtag)
      (inst mov tmp rbp-tn)
      (inst sub tmp rsp-tn)
      (inst add tmp fixnum-tag-mask)
      (inst and tmp (lognot fixnum-tag-mask))
      (storew tmp vectors
              (1+ vector-data-offset) other-pointer-lowtag)

      (when unboxed
        (loadw unboxed-vec vectors (+ 2 vector-data-offset)
               other-pointer-lowtag)
        (inst mov temp-reg-tn unboxed-vec)
        (inst and temp-reg-tn lowtag-mask)
        (inst cmp temp-reg-tn other-pointer-lowtag)
        (inst jmp :ne ALLOC-UNBOXED)

        (loadw temp-reg-tn unboxed-vec 0 other-pointer-lowtag)
        (inst cmp temp-reg-tn simple-array-unsigned-byte-64-widetag)
        (inst jmp :ne ALLOC-UNBOXED)

        (loadw temp-reg-tn unboxed-vec vector-length-slot other-pointer-lowtag)
        (inst cmp temp-reg-tn (fixnumize (length unboxed)))
        (inst jmp :ge UNBOXED-ALLOCED)
        (emit-label ALLOC-UNBOXED)
        (pseudo-atomic
         (allocation unboxed-vec (* 8
                                    (+ vector-data-offset
                                       (logand -2 (1+ (length unboxed))))))
         (inst lea unboxed-vec (make-ea :byte :base unboxed-vec
                                        :disp other-pointer-lowtag))
         (storew (fixnumize (length unboxed)) unboxed-vec
                 vector-length-slot other-pointer-lowtag)
         (storew simple-array-unsigned-byte-64-widetag unboxed-vec
                 0 other-pointer-lowtag))
        (storew unboxed-vec vectors (+ 2 vector-data-offset)
                other-pointer-lowtag)
        (emit-label UNBOXED-ALLOCED))

      (when boxed
        (loadw boxed-vec vectors (+ 3 vector-data-offset)
               other-pointer-lowtag)
        (inst mov temp-reg-tn boxed-vec)
        (inst and temp-reg-tn lowtag-mask)
        (inst cmp temp-reg-tn other-pointer-lowtag)
        (inst jmp :ne ALLOC-BOXED)

        (loadw temp-reg-tn boxed-vec 0 other-pointer-lowtag)
        (inst cmp temp-reg-tn simple-vector-widetag)
        (inst jmp :ne ALLOC-BOXED)

        (loadw temp-reg-tn boxed-vec vector-length-slot other-pointer-lowtag)
        (inst cmp temp-reg-tn (fixnumize (length boxed)))
        (inst jmp :ge BOXED-ALLOCED)
        (emit-label ALLOC-BOXED)
        (pseudo-atomic
         (allocation boxed-vec (* 8
                                  (+ vector-data-offset
                                     (logand -2 (1+ (length boxed))))))
         (inst lea boxed-vec (make-ea :byte :base boxed-vec
                                      :disp other-pointer-lowtag))
         (storew (fixnumize (length boxed)) boxed-vec
                 vector-length-slot other-pointer-lowtag)
         (storew simple-vector-widetag boxed-vec
                 0 other-pointer-lowtag))
        (storew boxed-vec   vectors (+ 3 vector-data-offset)
                other-pointer-lowtag)
        (emit-label BOXED-ALLOCED))
      
      (loop for i upfrom 0
            for tn in boxed
            do (inst mov tmp tn)
               (storew tmp boxed-vec (+ i vector-data-offset)
                       other-pointer-lowtag))

      (loop for i upfrom 0
            for tn in unboxed
            do
            (sc-case tn
              ((single-reg double-reg)
               (inst movd tmp tn))
              ((single-stack double-stack)
               (inst mov tmp (ea-for-df-stack tn)))
              (t (inst mov tmp tn)))
            (storew tmp unboxed-vec (+ i vector-data-offset)
                    other-pointer-lowtag))
      
      (unless (eq capture-target END)
        (inst jmp capture-target))
      (emit-label END)

      ;; restore sequence
      (assemble (*elsewhere*)
        (align n-fixnum-tag-bits #x90)
        (emit-label RESTORE)
        (inst mov rsp-tn rbp-tn)
        (inst sub rsp-tn (make-ea-for-object-slot
                          temp-reg-tn (+ 1 vector-data-offset)
                          other-pointer-lowtag))
        ;; restore unboxed
        (when unboxed
          (loadw unboxed-vec temp-reg-tn (+ 2 vector-data-offset)
                 other-pointer-lowtag)
          (loop for i upfrom 0
                for tn in unboxed
                do (loadw tmp unboxed-vec
                          (+ i 2) other-pointer-lowtag)
                (sc-case tn
                  ((single-reg double-reg)
                   (inst movd tn tmp))
                  ((single-stack double-stack)
                   (inst mov (ea-for-df-stack tn) tmp))
                  (t (inst mov tn tmp)))))
        ;; restore boxed
        (when boxed
          (loadw boxed-vec temp-reg-tn (+ 3 vector-data-offset)
                 other-pointer-lowtag)
          (loop for i upfrom 0
                for tn in boxed
                do (loadw tmp boxed-vec
                          (+ i 2) other-pointer-lowtag)
                (move tn tmp)))
        (inst mov vectors temp-reg-tn)
        (inst jmp restore-target)))))

(define-vop (%call-locals)
  (:policy :fast-safe)
  (:translate %call-locals)
  (:args (vector :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg) target)
  (:results)
  (:generator 10
    (inst mov temp-reg-tn vector)
    (loadw target temp-reg-tn vector-data-offset
           other-pointer-lowtag)
    (inst jmp target)))

(defun %call-locals (x)
  (declare (type (simple-array t (4)) x))
  (%call-locals x))

(defun call-locals (x)
  (%call-locals x))
