(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun listify (x)
    (if (listp x)
      x
      (list x)))

  (defun primobj-or-lose (name &key (test #'eq))
    (or (find name
              sb-vm:*primitive-objects*
              :key #'sb-vm:primitive-object-name
              :test test)
        (error "Unknown primitive object type: ~A" name))))

(declaim (inline sap-ref-lispobj))
(defun sap-ref-lispobj (sap &optional (offset 0))
  (declare (type sb-sys:system-area-pointer sap)
           (type (signed-byte #.sb-vm:n-word-bits) offset))
  (sb-kernel:%make-lisp-obj (sb-sys:sap-ref-word sap offset)))

(declaim (inline lispobj-sap))
(defun lispobj-sap (x)
  (sb-sys:int-sap (sb-kernel:get-lisp-obj-address x)))

(declaim (inline lispobj-ref-slot))
(defun lispobj-ref-slot (x slot)
  (declare (type (and unsigned-byte fixnum) slot))
  (let* ((aligned-addr (logandc2 (sb-kernel:get-lisp-obj-address x)
                                 sb-vm:lowtag-mask)))
    (sap-ref-lispobj (sb-sys:int-sap aligned-addr)
                     (* sb-vm:n-word-bytes slot))))

(declaim (inline real-widetag-of))
(defun real-widetag-of (x)
  (let* ((addr   (sb-kernel:get-lisp-obj-address x))
         (lowtag (logand addr sb-vm:lowtag-mask))
         (header (sb-sys:sap-ref-word (sb-sys:int-sap addr)
                                      (- lowtag))))
    (values (logand header sb-vm:widetag-mask)
            (ash header (- sb-vm:n-widetag-bits)))))

(defmacro %widetag-case ((object &optional default-op) &body specs)
  (let ((cases (make-hash-table)))
    (loop for (type . spec) in (mapcar 'listify specs)
          for case = (typecase type
                       (cons (cdr type))
                       (t (symbol-value
                           (sb-vm:primitive-object-widetag
                            (primobj-or-lose type :test #'string=)))))
          do (let ((case (if (member case '(t nil otherwise))
                           t
                           case)))
               (assert (null (gethash case cases)))
               (setf (gethash case cases)
                     (or spec `((,default-op ,type ,object))))))
    `(,(if (gethash t cases) 'case 'ecase) (real-widetag-of ,object)
       ,@(sort (loop
                 for tag being the hash-key in cases
                 using (hash-value spec)
                 collect `(,tag ,@spec))
               (lambda (x y)
                 (if (and (numberp x)
                          (numberp y))
                   (< x y)
                   (numberp x)))
               :key #'car))))

(defmacro tag-case ((object &optional default-op) &body specs)
  (let ((cases   (make-hash-table))
        (default nil)
        (_obj    (gensym "OBJ")))
    (loop for spec in (mapcar 'listify specs)
          for type = (first spec)
          for case = (typecase type
                       ((member t otherwise) type)
                       (cons                 (car type))
                       (t (symbol-value
                           (sb-vm:primitive-object-lowtag
                            (primobj-or-lose type :test #'string=)))))
          do (cond ((member case '(t otherwise nil))
                    (assert (null default))
                    (setf default (rest spec)))
                   (t (push spec (gethash case cases)))))
    `(let ((,_obj ,object))
       (,(if default 'case 'ecase) (sb-kernel:lowtag-of ,_obj)
         ,@(sort (loop
                   for tag being the hash-key in cases
                   using (hash-value specs)
                   collect `(,tag (%widetag-case (,_obj ,default-op)
                                    ,@(nreverse specs))))
                 #'< :key #'first)
         ,@(when default
             `((t ,@default)))))))

(defmacro with-primobj-data ((name
                              obj &key data length)
                             &body body)
  (let* ((objdef       (primobj-or-lose name :test #'string=))
         (var-length-p (sb-vm:primitive-object-variable-length-p objdef))
         (_obj         (gensym "OBJ")))
    (when (and var-length-p (not data))
      (setf data (gensym "DATA")))
    `(let* ((,_obj ,obj)
            ,@(when data
                `((,data (nth-value 1 (real-widetag-of ,_obj)))))
            ,@(when length
               `((,length ,(if var-length-p
                             `(1+ ,data)
                             (sb-vm:primitive-object-size objdef))))))
       (declare (ignorable ,_obj))
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-alien-variable page-table-pages unsigned-long)

  (define-alien-type page
      (struct page
              (region-start-offset unsigned-long :alignment 16)
              (bytes-used          unsigned-short)
              (bitmask             unsigned-char)
              (generation          char)
              (nil                 unsigned-int) ;; padding
              ))

  (define-alien-variable page-table (* page)))

(declaim (type (and unsigned-byte fixnum) +heap-base+ +page-bytes+))
(defconstant +heap-base+ (sb-kernel:current-dynamic-space-start))
(defconstant +page-bytes+ (sb-sys:get-page-size))

(declaim (inline dynamic-object-p))
(defun dynamic-object-p (x)
  (< (sb-kernel:current-dynamic-space-start)
     (sb-kernel:get-lisp-obj-address x)
     #+nil(sb-sys:sap-int (sb-kernel:dynamic-space-free-pointer))))

(declaim (inline object-generation))
(defun object-generation (x)
  (let* ((addr  (logandc2 (sb-kernel:get-lisp-obj-address x)
                          sb-vm:lowtag-mask))
         (index (truncate (truly-the
                           (unsigned-byte #.sb-vm:n-word-bits)
                           (- addr +heap-base+))
                          +page-bytes+))
         (page  (deref page-table index)))
    (values (slot page 'generation) index)))

(defun count-words (root &optional (generation-limit 5))
  (declare (type (and unsigned-byte fixnum) generation-limit))
  (let ((seenp (make-hash-table :test #'eql))
        (size  0))
    (declare (type (and unsigned-byte fixnum) size)
             (optimize speed))
    (macrolet ((simple-mark (type obj
                                  &rest to-mark)
                 (declare (optimize (speed 0)))
                 `(let ((obj ,obj))
                    (with-primobj-data (,type obj :length length)
                      (inc-size length)
                      ,@(loop with objdef = (primobj-or-lose type
                                                    :test #'string=)
                              for slot in (sb-vm:primitive-object-slots
                                             objdef)
                              for name    = (sb-vm:slot-name slot)
                              for options = (sb-vm:slot-options slot)
                              for offset  = (sb-vm:slot-offset slot)
                              for restp   = (sb-vm:slot-rest-p slot)
                              when (if to-mark
                                     (member name to-mark
                                             :test #'string=)
                                     (not (getf options :c-type)))
                                collect
                             (if restp
                               `(loop for i from ,offset below length
                                      do `(mark (lispobj-ref-slot obj i)))
                               `(mark (lispobj-ref-slot obj ,offset)))))))
               (mark-slots ((type obj)
                            &rest slots)
                 (declare (optimize (speed 0)))
                 `(let ((obj ,obj))
                    ,@(loop
                        with primobj = (primobj-or-lose
                                               type
                                               :test #'string=)
                        for slot in (sb-vm:primitive-object-slots
                                       primobj)
                        for name = (sb-vm:slot-name slot)
                        for offset = (sb-vm:slot-offset slot)
                        for restp = (sb-vm:slot-rest-p slot)
                        when (member name slots :test #'string=)
                          collect
                        `(mark (lispobj-ref-slot obj ,offset))))))
      (labels
          ((inc-size (word-count)
             (declare (type (and unsigned-byte fixnum) word-count))
             (setf size
                   (truly-the (and unsigned-byte fixnum)
                              (+ size
                                 (logandc2 (1+ word-count) 1)))))
           (mark-array (array)
             (etypecase array
               ((simple-array t 1)
                  (let ((length (length array)))
                    (inc-size (1+ length))
                    (loop for i below length
                          do (mark (aref array i)))))
               ((simple-array * 1)
                  (let* ((length (length array))
                         (saetp  (sb-c::find-saetp
                                  (array-element-type array)))
                         (nbits  (sb-vm:saetp-n-bits saetp))
                         (pad    (sb-vm:saetp-n-pad-elements saetp)))
                    (inc-size (1+ (ceiling (* (+ pad length)
                                              nbits)
                                           sb-vm:n-word-bits)))))
               (array
                  (simple-mark array array))))
           (mark-instance (instance)
             "This is where the cruft will be for a treeshaker..."
             (with-primobj-data (instance instance :length length)
               (inc-size length)
               ;; cruft.
               (when (typep instance `(or sb-pcl:slot-definition
                                          sb-pcl::class
                                          sb-thread::spinlock
                                          sb-thread::mutex
                                          sb-thread::thread
                                          sb-thread::waitqueue
                                          sb-thread::semaphore
                                          sb-thread::session
                                          sb-c:definition-source-location
                                          sb-c::info-env
                                          sb-kernel::condition-classoid))
                 ;; types that should be ignored or otherwise not marked
                 (format t "instance: ~A~%" instance)
                 (return-from mark-instance))

               ;; cut the marking off early. Don't need *much* more than
               ;; object identity
               (when (sb-kernel::layout-p instance)
                 ;; just save a dummy parent layout w/ the correct
                 ;; GC metadata
                 #+nil (mark (sb-kernel:%instance-layout instance))
                 (unless (gethash (sb-kernel:%instance-layout instance) seenp)
                   (inc-size 13) ; size of a layout
                   (setf (gethash (sb-kernel:%instance-layout instance) seenp)
                         t))
                 (mark (sb-kernel:layout-inherits instance))
                 #+nil(mark (sb-kernel:layout-slot-table instance))
                 #+nil(when (sb-pcl::wrapper-p instance) ; needed?
                   (mark (sb-pcl::wrapper-instance-slots-layout instance))
                   (mark (sb-pcl::wrapper-class-slots instance)))
                 (return-from mark-instance))
               
               (let* ((layout (sb-kernel:%instance-layout instance))
                      (nraw   (sb-kernel:layout-n-untagged-slots
                               layout)))
                 (loop for i from 1 below (- length nraw)
                       do (mark (lispobj-ref-slot instance i))))))
           (mark-code (code)
             (with-primobj-data (code code :length length)
               (let ((code-size (sb-kernel:%code-code-size code)))
                 (inc-size (+ length code-size -1))
                 (loop for i from 1 below (1- length)
                       unless (member
                                i
                                '#.(list
                                     sb-vm::code-debug-info-slot))
                         do (mark (lispobj-ref-slot code i)))
                 (do ((fun (sb-kernel:%code-entry-points code)
                        (sb-kernel:%simple-fun-next fun)))
                     ((null fun))
                   (setf (gethash fun seenp) t)
                   #+nil(mark-slots (simple-fun fun)
                                    ;; shouldn't fully mark these
                                    self next
                                    ;; name arglist type xrefs
                                    )))))
           (%mark (obj)
             (setf (gethash obj seenp) (hash-table-count seenp))
             #+nil(when (> (hash-table-count seenp) 1000)
               (return-from %mark nil))
             (tag-case (obj simple-mark)
               cons
               (instance
                (mark-instance obj))
               bignum
               ratio
               double-float
               complex
               (array
                (mark-array obj))
               (#. (cons sb-vm:other-pointer-lowtag
                         sb-vm:code-header-widetag)
                   (when (eq obj (load-time-value
                                  (sb-kernel:fun-code-header #'count-words)))
                     (format t "SELF!?~%")
                     (return-from %mark nil))
                   (mark-code obj))
               (fdefn
                (let ((name (sb-kernel:fdefn-name obj)))
                  (when (or
                         (member name '(sb-impl::%failed-aver
                                        sb-kernel:fdefinition-object
                                        sb-impl::encapsulation-info
                                        error))
                         (and (symbolp name)
                             (eq (symbol-package name)
                                 (load-time-value
                                  (find-package "SB-KERNEL")))
                             (search "-ERROR" (symbol-name name))))
                    (format t "error: ~A~%" obj)
                    (return-from %mark nil)))
                (simple-mark fdefn obj fun))
               (simple-fun
                ;; f-c-h must be computed from the header data
                ;; (offset from s-f's address)
                (let ((name (sb-kernel:%simple-fun-name obj)))
                  (when (and (symbolp name)
                             (search "-TYPE-METHOD" (symbol-name name)))
                    (format t "type-method: ~A~%" obj)
                    (return-from %mark nil)))
                (mark (sb-kernel:fun-code-header obj)))
               (closure
                (simple-mark closure obj info)
                ;; x86oids: closure-fun is a raw pointer to code
                ;; must subtract a couple words to find simple-fun
                (mark (sb-kernel:%closure-fun obj)))
               (funcallable-instance
                (simple-mark funcallable-instance obj
                             function info))
               value-cell
               sap
               weak-pointer
               (symbol
                ;; probably don't want to do packages & what not
                (simple-mark symbol obj
                             value plist #+nil name))
               complex-single-float
               complex-double-float
               (lutex nil)))
           (mark (obj)
             (when (and (oddp (sb-kernel:lowtag-of obj))
                        (dynamic-object-p obj)
                        (< (object-generation obj) generation-limit)
                        (not (gethash obj seenp)))
               (%mark obj))))
        (declare (inline #+nil mark inc-size)
                 (notinline %mark))
        (mark root)
        (values size seenp)))))
