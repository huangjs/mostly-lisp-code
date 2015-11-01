(in-package :cffi)

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-kernel:system-area-ub8-fill value pointer 0 length))

(declaim (inline copy-foreign-memory))
(defun copy-foreign-memory (src-ptr dst-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (sb-kernel:system-area-ub8-copy src-ptr 0 dst-ptr 0 length))

(defconstant +array-header-size+ (* 2 sb-vm:n-word-bytes))

(defun make-static-vector (size)
  "Create an (UNSIGNED-BYTE 8) simple vector of size SIZE which will
not be moved by the garbage collector. The vector might be allocated in
foreign memory so you must always call FREE-STATIC-VECTOR to free it."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note)
           (optimize speed))
  (check-type size alexandria:non-negative-fixnum)
  (let* ((allocation-size (+ size +array-header-size+))
         (memblock (foreign-alloc :char :count allocation-size)))
    (cond
      ((null-pointer-p memblock)
       ;; FIXME: signal proper error condition
       (error "Cannot allocate foreign memory!"))
      (t
       ;; the malloc'd memory must be aligned on a 2-word boundary
       ;; I'd expect malloc() to return a properly aligned pointer everywhere
       ;; but I'm not certain. SIONESCU 20090505
       (assert (zerop (mod (pointer-address memblock) (* 2 
							 sb-vm:n-word-bytes))))
       (fill-foreign-memory memblock allocation-size 0)
       (let ((type-tag sb-vm:simple-array-unsigned-byte-8-widetag)
             (length (sb-vm:fixnumize size)))
         (setf (mem-aref memblock :int 0) type-tag
               (mem-aref memblock :int 1) length)
         (sb-kernel:%make-lisp-obj (logior (pointer-address memblock)
                                           sb-vm:other-pointer-lowtag)))))))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector)
  "Return a foreign pointer to VECTOR's data.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (make-pointer (sb-kernel:get-lisp-obj-address vector)))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR if allocated in foreign memory."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((pointer (static-vector-pointer vector)))
    (foreign-free (inc-pointer pointer (- 1 +array-header-size+)))))

(defmacro with-static-vector ((ptr-var size) &body body)
  "Bind PTR-VAR to a static vector of size SIZE and execute BODY
within its dynamic extent. The static vector is freed upon exit."
  (alexandria:with-gensyms (static-vector)
    `(let* ((,static-vector (make-static-vector ,size))
            (,ptr-var (static-vector-pointer ,static-vector)))
       (unwind-protect
            (progn ,@body)
         (free-static-vector ,static-vector)))))
