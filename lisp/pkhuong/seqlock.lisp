;;; SeqLock + spinlock-based STM
;;;
;;; A nearly verbatim copy from "Transactional Mutex Locks", by
;;; Dalessandro, Dice, Scott, Shavit and Spear
;;; (http://www.cs.rochester.edu/u/scott/papers/2010_EuroPar_TML.pdf).
;;;
;;; The big idea is to use a single global SeqLock for everything.
;;; SeqLocks are very lightweight, and, unlike spinlocks, allow
;;; read parallelism and only incur writes to shared memory on writes.
;;; They are used for read-mostly workloads in, e.g., Linux.
;;;
;;; Only using a SeqLock results in a single uncancellable writer...
;;; consequently, there is no bookkeeping to rollback transactions.
;;;
;;; Interestingly, it also plays well with specialised schemes that
;;; do allow rollbacks.  The code below is an example of that.
;;;
;;; The current implementation only has one transactional type:
;;; the untyped CELL.  However, it offers two concurrency control
;;; options.
;;;
;;; The default uses the global SeqLock, as described in TML. This
;;; is ideal for read-mostly locations, especially when there will
;;; be parallel readers or memory allocation must be minimized.
;;; Note that, since the locations that were written aren't logged,
;;; spurious conflicts are very likely.
;;;
;;; Using the second optional argument to MAKE-CELL, one can also
;;; specify that the newly created cell should be lock-based.  The
;;; current spinlock doesn't allow parallel readers, but is very
;;; lightweight.  It should be used for locations that are write
;;; heavy and will otherwise lead to spurious conflicts.  Sharing
;;; a single lock across multiple cells can reduce consing, and
;;; help avoid conflicts.
;;;
;;; This cooperation betweem log-based and SeqLock-based STMs is
;;; implemented by considering the SeqLock writer as the only inexorable
;;; (uncancellable) thread.  Every other thread can only wait a bounded
;;; amount of time before aborting.
;;;
;;; Arbitrary effects are easily supported, by calling MAKE-INEXORABLE
;;; before executing any.
;;; Similarly, arbitrary lookups can be supported by calling CHECK-READ
;;; between the lookup and the use of any looked-up value.
;;;
;;; USAGE:
;;;
;;; MAKE-CELL initial-value &optional lock-p
;;;  Conses a new transactional cell, with the given initial value.
;;;  If lock-p is T, a fresh lock is assigned to the cell. If it's
;;;  a lock (as returned by executing (MAKE-LOCK)), that lock is
;;;  reused for the cell. If it's NIL (the default), the global
;;;  SeqLock will be used.
;;;
;;; CALL-WITH-TRANSACTION thunk
;;;  Executes thunk within a transaction. On failures, thunk may be
;;;  executed multiple times. Since the whole scheme is based on
;;;  spinlocks, only 32 threads are allowed to be inside
;;;  CALL-WITH-TRANSACTION concurrently.
;;;  Nested transactions are supported by flattening everything into
;;;  the toplevel transaction.
;;;  ** There is no error handling ** If your transactional code is
;;;  broken, the whole system can go down.
;;;
;;;
;;; WITH-TRANSACTION &body body
;;;  Wraps body in a thunk for CALL-WITH-TRANSACTION.
;;;
;;; CELL-VALUE and (SETF CELL-VALUE)
;;;  Can only be used in the dynamic extent of CALL-WITH-TRANSACTION.
;;;  Perform transactional reads and writes to cells.

(declaim (type (or null lock) lock))
(defparameter *locks* nil)
(defstruct (lock
             (:constructor make-lock ()))
  (owner nil :type (or null sb-thread:thread))
  (next  nil :type (or null lock)))
(declaim (freeze-type lock))

(defun get-lock (lock waitp)
  (declare (type lock lock))
  (let ((self sb-thread:*current-thread*))
    (when (eq self (lock-owner lock))
      (return-from get-lock))
    (flet ((inner-loop ()
             (declare (optimize speed))
             (dotimes (i 1024)
               (when (and (null (lock-owner lock))
                          (null (compare-and-swap (lock-owner lock)
                                                  nil
                                                  self)))
                 (shiftf (lock-next lock)
                         *locks*
                         lock)
                 (return t)))))
      (cond (waitp
             (loop until (inner-loop)))
            ((inner-loop))
            (t
             (throw 'abort-transaction t))))))

(defun release-lock (lock)
  (declare (type lock lock))
  (assert (eq (lock-owner lock) sb-thread:*current-thread*))
  (setf (lock-owner lock) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct seqlock
    (number 0 :type fixnum)))

(defstruct (cell
             (:constructor make-cell (%value &optional lock-p
                                      &aux
                                      old-value
                                      (lock (case lock-p
                                              (nil nil)
                                              (t   (make-lock))
                                              (otherwise lock-p)))
                                      (next-by-owner nil))))
  %value
  old-value
  (lock nil :type (or null lock) :read-only t)
  (next-by-owner nil :type (or null cell)))

(declaim (type seqlock **seqlock**)
         (type fixnum *seq-number*)
         (type (or null cell) *actions*))
(defglobal **seqlock** (make-seqlock))
(defparameter *seq-number* -1)
(defparameter *actions* nil)

(declaim (inline inexorable-p))
(defun inexorable-p (&optional (number *seq-number*))
  (oddp number))

(declaim (maybe-inline acquire-write check-read))
(defun acquire-write ()
  (declare (optimize speed))
  (when (inexorable-p)
    (return-from acquire-write t))
  (let* ((seqlock **seqlock**)
         (num  (seqlock-number seqlock))
         (next (logand (1+ num) most-positive-fixnum)))
    (cond ((and (eql num *seq-number*)
                (eql num (compare-and-swap
                          (seqlock-number seqlock)
                          num
                          next)))
           (setf *seq-number* next)
           t)
          (t (throw 'abort-transaction t)))))

(defun make-inexorable ()
  (acquire-write))

(defun release-write (&aux (seqlock **seqlock**))
  (assert (inexorable-p))
  (assert (eql *seq-number* (seqlock-number seqlock)))
  (incf (seqlock-number seqlock))
  (setf *seq-number* -1)
  t)

(defun check-read ()
  (declare (optimize speed))
  (let ((number *seq-number*))
    (unless (or (inexorable-p number)
                (eql (seqlock-number **seqlock**) number))
      (throw 'abort-transaction nil))))

(defun cell-value (cell)
  (declare (type cell cell)
           (optimize speed))
  (assert (/= -1 *seq-number*))
  (let ((lock (cell-lock cell)))
    (cond (lock
           (get-lock lock (inexorable-p))
           (cell-%value cell))
          (t
           (prog1 (cell-%value cell)
             (check-read))))))

(defun (setf cell-value) (value cell)
  (declare (type cell cell)
           (optimize speed))
  (assert (/= -1 *seq-number*))
  (let ((lock (cell-lock cell)))
    (cond (lock
           (get-lock lock (inexorable-p))
           (shiftf (cell-next-by-owner cell)
                   *actions*
                   cell)
           (shiftf (cell-old-value cell)
                   (cell-%value cell)
                   value)
           value)
          (t
           (acquire-write)
           (setf (cell-%value cell) value)))))

(defun release-locks ()
  (do ((lock *locks*
         (shiftf (lock-next lock) nil)))
      ((null lock))
    (release-lock lock)))

(defun commit-transaction ()
  (do ((cell *actions*
         (shiftf (cell-next-by-owner cell) nil)))
      ((null cell)
         (release-locks))
    (setf (cell-old-value cell) nil)))

(defun abort-transaction ()
  (do ((cell *actions*
         (shiftf (cell-next-by-owner cell) nil)))
      ((null cell)
         (release-locks))
    (shiftf (cell-%value cell)
            (cell-old-value cell)
            nil)))

(defglobal **transaction-semaphore** (sb-thread:make-semaphore :count 32))

(defun call-with-transaction (fun)
  (when (/= *seq-number* -1)
    (return-from call-with-transaction (funcall fun)))
  (let ((seqlock **seqlock**)
        (must-acquire nil))
    (sb-thread:wait-on-semaphore **transaction-semaphore**)
    (loop
      (let ((*seq-number* (loop
                            (let ((num (seqlock-number seqlock)))
                              (when (evenp num)
                                (return num)))))
            (*actions* nil)
            (*locks*   nil))
        (when must-acquire
          (acquire-write))
        (catch 'abort-transaction
          (return-from call-with-transaction
            (multiple-value-prog1 (funcall fun)
              (commit-transaction)
              (when (inexorable-p)
                (release-write))
              (sb-thread:signal-semaphore **transaction-semaphore**))))
        (abort-transaction)
        (setf must-acquire t)))))

(defmacro with-transaction (&body body)
  `(flet ((.transaction-body. ()
            ,@body))
     (call-with-transaction #'.transaction-body.)))
