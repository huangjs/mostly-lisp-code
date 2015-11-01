(in-package "STM")
;;; # SeqLock infrastructure
;;;
;;; A seqlock is simply a CASable location that stores small integers.
;;;
;;; There are four basic operations:
;;;
;;; - Initialise the thread-local sequence number (get-sequence-number).
;;;
;;; - Verify that the global sequence number is consistent with the
;;;   thread-local one (check-read).
;;;
;;; - Acquire exclusive write rights (acquire-write) if the sequence
;;;   numbers are still consistent.
;;;
;;; - Relinquish write rights (release-irrevocable).
;;;
;;; Finally, as an optimization, init-irrevocable allows one to initialise
;;; the local sequence number while acquiring write rights.
;;;
;;; These operations are always successful; on failure, they directly
;;; abort the current transaction.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct seqlock
    (number 0 :type fixnum))
  (declaim (freeze-type seqlock)))

;;; The global seqlock for STM
(declaim (type seqlock **seqlock**))
(defglobal **seqlock** (make-seqlock))
(define-symbol-macro %seqlock%
    (truly-the seqlock
               (opaque-identity (load-time-value **seqlock** t))))

(definline-macro get-sequence-number (&aux (seqlock %seqlock%))
  (loop
    (let ((num (seqlock-number seqlock)))
      (when (evenp num)
        (return num)))))

;;; A thread is irrevocable when it has acquired the write bit on
;;; the global SeqLock.  There is only at most one such thread
;;; (transaction) at any one time.  Only that thread never balks
;;; when waiting on resources.  Unfortunately, an irrevocable thread
;;; also results in spurious conflicts...
(definline-macro irrevocable-p (&optional (number %seq-number%))
  (oddp number))

;;; The usual case is that a thread attempts to become irrevocable,
;;; or aborts if it can't.
(defun %become-irrevocable (seqlock seq-number)
  (let* ((num  (seqlock-number seqlock))
         (next (logand (1+ num) most-positive-fixnum)))
    (if (and (eql num seq-number)
             (eql num (compare-and-swap
                       (seqlock-number seqlock)
                       num
                       next)))
        next
        (abort-transaction t))))

(definline-macro become-irrevocable ()
  (with-transaction-data
    (when (irrevocable-p)
      (return-from become-irrevocable))
    (setf %seq-number% (%become-irrevocable %seqlock% %seq-number%)))
  nil)

(definline-macro acquire-write ()
  (become-irrevocable))

(defglobal **irrevocable-mutex** (sb-thread:make-mutex
                                  :name "Irrevocable transaction mutex"))
;;; After aborts, however, it may make sense to initialise a thread
;;; as irrevocable from the get go.
(defun init-irrevocable ()
  (let ((seqlock %seqlock%)
        (count   0))
    (declare (type fixnum count))
    (flet ((acquire ()
             (let* ((num  (seqlock-number seqlock))
                    (next (logand (1+ num) most-positive-fixnum)))
               (and (evenp num)
                    (eql num (compare-and-swap
                              (seqlock-number seqlock)
                              num
                              next))
                    next))))
      (or (loop repeat (ash 1 22)
                thereis (acquire))
          (sb-thread:with-mutex (**irrevocable-mutex**)
            (acquire))))))

(defun release-irrevocable (&aux (seqlock %seqlock%))
  (with-transaction-data
    (let ((seq-number %seq-number%))
      (assert (irrevocable-p))
      (compare-and-swap (seqlock-number seqlock)
                        seq-number
                        (logand (1+ seq-number) most-positive-fixnum))
      t)))

;;; After any read, it's important to check that the sequence number
;;; hasn't changed.
(definline-macro check-read (value)
  (declare (optimize speed))
  (with-transaction-data
    (let ((number %seq-number%))
      (unless (or (irrevocable-p number)
                  (eql (seqlock-number %seqlock%) number))
        (abort-transaction nil))))
  value)
