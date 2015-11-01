;;; FD-based flag. Multiple wakers, single waiter.
;;;
;;; Implements an interface similar to java.util.concurrent.locks.LockSupport
;;;
;;; A single thread can wait on it, and any number of thread can wake
;;; the waiter up.

(defconstant +buffer-size+ 1024)

(define-alien-type pollfd
    (struct pollfd
            (fd int)
            (events short)
            (revents short)))

(defstruct (wake-flag
             (:constructor %make-wake-flag (rfd wfd pollfd buffer))
             (:print-object print-wake-flag))
  (rfd 0 :type (signed-byte 32) :read-only t)
  (wfd 0 :type (signed-byte 32) :read-only t)
  (pollfd nil :type (alien (* pollfd)) :read-only t)
  (buffer nil :type system-area-pointer :read-only t))
(declaim (freeze-type wake-flag))

(defun print-wake-flag (object stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "fd: ~S ~S" (wake-flag-rfd object) (wake-flag-wfd object))))

(defun make-wake-flag ()
  (multiple-value-bind (rfd wfd)
      (let ((fds (make-array 2 :element-type '(signed-byte 32))))
        (declare (dynamic-extent fds))
        (sb-posix:pipe fds))
    (sb-posix:fcntl rfd sb-posix:f-setfl sb-posix:o-nonblock)
    (sb-posix:fcntl wfd sb-posix:f-setfl sb-posix:o-nonblock)
    (let* ((pollfd (make-alien pollfd))
           (buffer (make-alien char +buffer-size+))
           (flag (%make-wake-flag rfd wfd pollfd (alien-sap buffer))))
      (setf (slot pollfd 'fd)      rfd
            (slot pollfd 'events)  1     ; should be grovelled (:
            (slot pollfd 'revents) 0)
      (finalize flag (lambda ()
                       (sb-posix:close rfd)
                       (sb-posix:close wfd)
                       (free-alien pollfd)
                       (free-alien buffer)))
      flag)))

(defun wake-wake-flag (flag)
  (declare (wake-flag flag)
           (optimize speed)
           (inline sb-unix:unix-write))
  (sb-unix:unix-write (wake-flag-wfd flag) (wake-flag-buffer flag) 0 1)
  t)

(declaim (maybe-inline poll))
(define-alien-routine poll int
  (pollfds (* pollfd)) (nfds unsigned) (timeout int))

(defun wait-wake-flag (flag &optional timeout)
  (declare (wake-flag flag)
           (optimize speed)
           (inline poll sb-unix:unix-read))
  (let ((timeout (or timeout -1)))
    (declare (type (signed-byte 32) timeout))
    (loop for ret = (poll (wake-flag-pollfd flag) 1 timeout)
          do
       (case ret
         (0 (return-from wait-wake-flag t))
         #+nil
         (-1 (error "error in poll(2)"))
         (t (return)))))
  (let ((rfd (wake-flag-rfd flag)))
    (loop for n = (sb-unix:unix-read rfd
                                     (wake-flag-buffer flag)
                     +buffer-size+)
          while (eql n +buffer-size+)))
  t)

;;; Example application: simple and unproven CLH lock.
;;;
;;; Can we make this first a spin lock that upgrades into
;;; a CLH lock on contention (and maybe even downgrades back
;;; when we release and no one's waiting)?

(defstruct (clh-lock
             (:constructor make-clh-lock ()))
  (queue      (list nil))
  (owner-cons nil))
(declaim (freeze-type clh-lock))

(defun get-clh (lock flag)
  (declare (type clh-lock lock)
           (type wake-flag flag))
  (let* ((qnode (list t))
         (last
          (loop
            (let ((last (clh-lock-queue lock)))
              (setf (cdr qnode) last)
              (when (eq last
                        (compare-and-swap (clh-lock-queue lock) last qnode))
                (return last))))))
    (block acquire
      (loop repeat (ash 1 16)
            unless (car last)
              do (return-from acquire))
      (loop
        (let ((must-wait (car last)))
          (when (null must-wait) (return-from acquire))
          (assert (eq must-wait t))
          (case (compare-and-swap (car last) t flag)
            ((nil) (return-from acquire))
            ((t) (return)))))
      (loop while (car last)
            do (wait-wake-flag flag)))
    (setf (clh-lock-owner-cons lock) qnode))
  t)

(defun release-clh (lock)
  (declare (type clh-lock lock))
  (let ((qnode (clh-lock-owner-cons lock)))
    (setf (cdr qnode) nil)
    (loop
      (let ((flag (car qnode)))
        (cond ((eq flag t)
               (when (eq (compare-and-swap (car qnode) t nil) t)
                 (return)))
              (t
               (assert (wake-flag-p flag))
               (setf (car qnode) nil)
               (wake-wake-flag flag)
               (return))))))
  t)
