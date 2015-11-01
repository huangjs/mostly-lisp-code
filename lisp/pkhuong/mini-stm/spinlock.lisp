(in-package "STM")
(defstruct (lock
             (:constructor make-lock ()))
  (owner nil :type (or null thread-id)))
(declaim (freeze-type lock))

(defun get-lock (lock waitp)
  (declare (type lock lock))
  (let ((self %thread-id%))
    (when (eq self (lock-owner lock))
      (return-from get-lock nil))
    (flet ((inner-loop ()
             (declare (optimize speed))
             (dotimes (i 1024)
               (when (and (null (lock-owner lock))
                          (null (compare-and-swap (lock-owner lock)
                                                  nil
                                                  self)))
                 (return t)))))
      (cond ((inner-loop))
            (waitp
             (loop until (inner-loop)
                   finally (return t)))
            (t
             (abort-transaction t))))))

(defun release-lock (lock)
  (declare (type lock lock))
  (let ((self %thread-id%))
    (assert (eq (lock-owner lock) self))
    (loop while
          (and (eq (lock-owner lock) self)
               (not (eq self
                        (compare-and-swap (lock-owner lock) self nil))))))
  nil)
