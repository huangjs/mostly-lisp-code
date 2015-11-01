;;; Byte R/W lock: Implement "TLRW: Return of the Read-Write Lock", by
;;; Dave Rice and Nir Shavit
;;; (http://blogs.sun.com/dave/resource/Transact2009-TLRW-ByteLock-Feb6.pdf).
;;;
;;; The implementation is very hacky.  Alignment to a cache line is
;;; useful for performance.  However, even more important is avoiding
;;; indirection.  To attain both these goals, bytelocks are allocated
;;; with posix_memalign, and the address represented as a fixnum; there
;;; is no GC for foreign memory, and we can't attach a finalizer to
;;; a fixnum.  Instead, the bytelock also includes a reference count
;;; that should be decremented when a reference to it is GCed.
;;;
;;; Note that this means that you must always maintain a reference
;;; to the bytelock live while it's locked.
;;;
;;; To create a bytelock, use make-bytelock, and finalize its references
;;; with the closure returned by bytelock-cleaner (with the bytelock
;;; as an argument).  Multiple finalizers are ok: the bytelock maintains
;;; an internal reference count.
;;;
;;; bytelock-read acquires a bytelock for reading (or aborts trying),
;;; bytelock-write acquires or upgrades a bytelock for writing, and
;;; release-bytelock releases any right acquired on a bytelock.

(in-package "STM")
(deftype bytelock ()
  "Byte lock address, cast as a fixnum"
  'fixnum)

(define-alien-type bytelock-data
    (struct bytelock-data
            (bytes (array unsigned-char #.(1+ +thread-count+)))
            (count unsigned-long)))

(declaim (inline posix-memalign))
(define-alien-routine posix-memalign int
  (memptr (* (* t)))
  (alignment unsigned-long)
  (size unsigned-long))

(defmacro with-bytelock-data ((var bytelock) &body body)
  (let ((sap (gensym "SAP")))
    `(let ((,sap (sb-sys:int-sap
                  (sb-kernel:get-lisp-obj-address ,bytelock))))
       (symbol-macrolet ((,var (sap-alien ,sap bytelock-data)))
         ,@body))))

(defun %allocate-bytelock ()
  (with-alien ((ptr (* t)))
    (assert (zerop (posix-memalign (addr ptr)
                                   64
                                   (alien-size bytelock-data :bytes))))
    (sb-kernel:%make-lisp-obj (sb-sys:sap-int (alien-sap ptr)))))

(defun %free-bytelock (bytelock)
  (declare (type bytelock bytelock))
  (with-bytelock-data (bytelock bytelock)
    (free-alien bytelock)))

(defun %init-bytelock (bytelock)
  (declare (type bytelock bytelock))
  (with-bytelock-data (bytelock bytelock)
    (sb-kernel:system-area-ub8-fill
     0
     (alien-sap bytelock)
     0
     (alien-size bytelock-data :bytes))))

(defun %print-bytelock (bytelock &optional (s t))
  (declare (type bytelock bytelock))
  (with-bytelock-data (bytelock bytelock)
    (symbol-macrolet ((bytes (slot bytelock 'bytes)))
      (format s "#<~S owner: ~A " 'bytelock (deref bytes 0))
      (format s "flags:")
      (loop for i from 1 upto +thread-count+
            do (format s " ~A" (deref bytes i)))
      (format s " count: ~A>" (slot bytelock 'count))))
  (values))

(defun empty-bytelock-p (bytelock)
  (declare (type bytelock bytelock))
  (with-bytelock-data (bytelock bytelock)
    (let ((sap (alien-sap (addr (slot bytelock 'bytes)))))
      (loop for i from 0 below 64 by 8
            always (zerop (sb-sys:sap-ref-64 sap i))))))

(defun %inc-bytelock (bytelock)
  (declare (type bytelock bytelock))
  (with-bytelock-data (bytelock bytelock)
    (let ((sap (alien-sap (addr (slot bytelock 'count)))))
      (xadd-word-sap sap 1)))
  nil)

(defun %dec-bytelock (bytelock)
  (declare (type bytelock bytelock))
  (with-bytelock-data (bytelock bytelock)
    (let* ((sap (alien-sap (addr (slot bytelock 'count))))
           (old (xadd-word-sap  sap (ldb (byte 64 0) -1))))
      (when (and (= old 1))
        (free-alien bytelock)
        t))))

(defun make-bytelock ()
  (let ((bytelock (%allocate-bytelock)))
    (%init-bytelock bytelock)
    (%inc-bytelock bytelock)
    bytelock))

(defun bytelock-cleaner (bytelock)
  (declare (type bytelock bytelock))
  (lambda () (%dec-bytelock bytelock)))

(defun bytelock-read (bytelock waitp &aux (thread (1+ %thread-id%)))
  (declare (type bytelock bytelock)
           (optimize speed))
  (with-bytelock-data (bytelock bytelock)
    (symbol-macrolet ((bytes (slot bytelock 'bytes)))
      (flet ((acquire ()
               (unless (zerop (deref bytes 0))
                 (return-from acquire nil))
               (setf (deref bytes thread) 1)
               (membar)
               (cond ((zerop (deref bytes 0)))
                     (t
                      (setf (deref bytes thread) 0)
                      nil))))
        (cond ((or (= thread (deref bytes 0))
                   (not (zerop (deref bytes thread))))
               nil)
              ((acquire))
              (waitp
               (loop until (acquire)
                     finally (return t)))
              (t (abort-transaction nil)))))))

(defun bytelock-write (bytelock waitp &aux (thread (1+ %thread-id%)))
  (declare (type bytelock bytelock)
           (optimize speed))
  (with-bytelock-data (bytelock bytelock)
    (symbol-macrolet ((bytes (slot bytelock 'bytes)))
      (when (= thread (deref bytes 0))
        (return-from bytelock-write nil))
      (flet ((acquire (waitp)
               (cond ((= thread (deref bytes 0)))
                     ((and (zerop (deref bytes 0))
                           (zerop (cas-byte-sap
                                   (alien-sap (addr bytes))
                                   0 thread))))
                     (waitp
                      (loop until (and (zerop (deref bytes 0))
                                       (zerop (cas-byte-sap
                                               (alien-sap (addr bytes))
                                               0 thread)))
                            finally (return t)))))
             (wait (waitp)
               (loop
                 for i from (- (1+ +thread-count+) 8) downto 0 by 8
                 always
                 (or (zerop (sap-ref-64 (alien-sap (addr bytes)) i))
                     (loop
                       for j from (max 1 i) below (+ i 8)
                       always
                       (or (= j thread)
                           (if waitp
                               (loop thereis (zerop (deref bytes j)))
                               (loop repeat 32
                                     thereis (zerop (deref bytes j))))))))))
        (cond ((and (acquire waitp)
                    (wait waitp))
               (cond ((zerop (deref bytes thread)))
                     (t
                      (setf (deref bytes thread) 0)
                      nil)))
              (t
               (assert (not waitp))
               (when (= thread (deref bytes 0))
                 (setf (deref bytes 0) 0))
               (abort-transaction t)))))))

(defun release-bytelock (bytelock)
  (declare (type bytelock bytelock)
           (optimize speed))
  (let ((thread (1+ %thread-id%)))
    (with-bytelock-data (bytelock bytelock)
      (symbol-macrolet ((bytes (slot bytelock 'bytes)))
        (cond ((= thread (deref bytes 0))
               (setf (deref bytes 0) 0))
              ((not (zerop (deref bytes thread)))
               (setf (deref bytes thread) 0))
              (t (error "Releasing a byte lock without owning it."))))))
  nil)
