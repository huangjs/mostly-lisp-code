;;; A basic transactional mutable cell data type.
;;;
;;; The simplest type of cell only uses the Seqlock for concurrency
;;; control.  It's basically a tiny wrapper around check-read and
;;; acquire-write.
;;;
;;; There are also two lock-ful cell types that show how OO-STM
;;; style operations can be supported.
;;;
;;; locked-cell is controlled with a spinlock.  It doesn't allow
;;; any read-parallelism, but is very simple.
;;;
;;; rw-cell is controlled with a read-write bytelock.  While it allows
;;; parallel reads, logging conses a tiny bit.
;;;
;;; Neither of these types use %locks%, since they both use dedicated
;;; locks, but the generalization should be clear.
;;;
;;; Instantiate a mutable cell with make-cell; the first argument is the
;;; initial value, and the optional second determines the type of cell
;;; to build.  The default (nil) creates a Seqlock cell;  passing t or
;;; :spin creates a locked-cell; passing :rw or :byte creates a rw-cell.
;;;
;;; The key to using locks in conjunction with the seqlock is to use
;;; irrevocable-p.  If it's false, any wait must be bounded, and if the
;;; limit is attained, the transaction aborts.  If it's true, the we
;;; must wait until the acquisition succeeds: non-rollbackable actions
;;; may have been executed.
(in-package "STM")
(defstruct (cell-base
             (:include action)
             (:constructor nil))
  %value)
(defstruct (cell
             (:include cell-base)
             (:constructor %make-cell)))
(defstruct (locked-cell
             (:include cell-base)
             (:constructor %make-locked-cell))
  (old-value nil)
  (lock nil :type (or null thread-id))
  (next nil :type t))
(defstruct (rw-cell
             (:include cell-base)
             (:constructor %make-rw-cell))
  (old-value nil)
  (lock nil :type bytelock))
(declaim (freeze-type cell-base cell locked-cell rw-cell))

(defun cell-action (cell step)
  (declare (ignore cell step))
  (error "~S called, but regular cells never roll back" 'cell-action))

(defun locked-cell-action (cell step)
  (declare (type locked-cell cell))
  (prog1 (shiftf (locked-cell-next cell) nil)
    (unless (eql (locked-cell-old-value cell) (locked-cell-%value cell))
      (ecase step
        (commit
           (setf (locked-cell-old-value cell) (locked-cell-%value cell)))
        (rollback
           (setf (locked-cell-%value cell)    (locked-cell-old-value cell)))))
    (setf (locked-cell-lock cell) nil)))

(defun rw-cell-action (cell step)
  (declare (type rw-cell cell))
  (unless (eql (rw-cell-old-value cell)  (rw-cell-%value cell))
    (ecase step
      (commit
         (setf (rw-cell-old-value cell)  (rw-cell-%value cell)))
      (rollback
         (setf (rw-cell-%value cell)     (rw-cell-old-value cell)))))
  (membar)
  (release-bytelock (rw-cell-lock cell)))

(defun make-cell (value &optional lock-p)
  (ecase lock-p
    ((nil)
       (%make-cell :fun #'cell-action :%value value))
    ((:byte :rw)
       (let ((cell (%make-rw-cell :fun #'rw-cell-action
                                  :%value value :old-value value
                                  :lock (make-bytelock))))
         (finalize cell (bytelock-cleaner (rw-cell-lock cell)))
         cell))
    ((t :spin)
       (%make-locked-cell :fun #'locked-cell-action
                          :%value value :old-value value))))

(defun get-cell-lock (cell)
  (declare (type locked-cell cell))
  (unless (eq %thread-id% (locked-cell-lock cell))
    (flet ((acquire ()
             (loop
               repeat 1024
               do (when (and (null (locked-cell-lock cell))
                             (null (compare-and-swap
                                    (locked-cell-lock cell)
                                    nil %thread-id%)))
                    (return t)))))
      (cond ((acquire))
            ((irrevocable-p)
             (loop until (acquire)))
            (t
             (abort-transaction t)))
      (shiftf (locked-cell-next cell)
              %actions%
              cell)))
  t)

(defun get-cell-read (cell)
  (declare (type rw-cell cell))
  (when (bytelock-read (rw-cell-lock cell) (irrevocable-p))
    (push cell %actions%))
  nil)

(defun get-cell-write (cell)
  (declare (type rw-cell cell))
  (when (bytelock-write (rw-cell-lock cell) (irrevocable-p))
    (push cell %actions%))
  nil)

(definline-macro cell-value (cell)
  (declare (type cell-base cell)
           (optimize speed))
  (with-transaction-data
    (etypecase cell
      (cell
         (check-read (cell-%value cell)))
      (locked-cell
         (get-cell-lock cell)
         (locked-cell-%value cell))
      (rw-cell
         (get-cell-read cell)
         (rw-cell-%value cell)))))

(definline-macro (setf cell-value) (value cell)
  (declare (type cell-base cell)
           (optimize speed))
  (with-transaction-data
    (etypecase cell
      (cell
         (acquire-write)
         (setf (cell-%value cell) value))
      (locked-cell
         (get-cell-lock cell)
         (setf (locked-cell-%value cell) value))
      (rw-cell
         (get-cell-write cell)
         (setf (rw-cell-%value cell) value)))))
