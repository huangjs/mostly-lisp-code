;;; The main STM function.
;;;
;;; Call CALL-WITH-TRANSACTION with a thunk as argument to execute
;;; the thunk inside a transaction (as many times as needed until
;;; success).  Nested transactions are implemented by simple
;;; flattening.
;;;
;;; The macro WITH-TRANSACTION is a slightly friendlier wrapper around
;;; CALL-WITH-TRANSACTION.
;;;
;;; The hooks *transaction-init-hooks*, *transaction-rollback-hooks* and
;;; *transaction-end-hooks* are provided, mostly to initialize, reinitialize
;;; and clean thread-local stuff.  When they are called, %thread-id%,
;;; has been initialized.

(in-package "STM")

(declaim (type list *transaction-init-hooks*
                    *transaction-rollback-hooks*
                    *transaction-end-hooks*))
(defvar *transaction-init-hooks* nil)
(defvar *transaction-rollback-hooks* nil)
(defvar *transaction-end-hooks* nil)

(declaim (type fixnum **aborts**))
(defglobal **aborts** 0)

(declaim (maybe-inline call-with-transaction))
(defun call-with-transaction (fun)
  (when (in-transaction-p)
    (return-from call-with-transaction (funcall fun)))
  (let ((must-acquire nil)
        (*thread-id* (get-thread-id)))
    (with-transaction-data
      (setf %actions% +last-action+
            %locks%   +last-action+)
      (map nil #'funcall *transaction-init-hooks*)
      (loop
        (setf %seq-number% (if must-acquire
                               (init-irrevocable)
                               (get-sequence-number))
              %transaction-id% (logand most-positive-fixnum
                                       (1+ %transaction-id%)))
        (let ((abort-condition
               (catch 'abort-transaction
                 (return
                   (multiple-value-prog1 (funcall fun)
                     (send-all-actions %actions% 'commit)
                     (send-all-actions %locks% 'release)
                     (setf %actions% +last-action+
                           %locks%   +last-action+)
                     (map nil #'funcall *transaction-end-hooks*)
                     (when (irrevocable-p)
                       (release-irrevocable))
                     (release-thread-id %thread-id%))))))
          (send-all-actions %actions% 'rollback)
          (send-all-actions %locks% 'release)
          (setf %actions% +last-action+
                %locks%   +last-action+)
          (map nil #'funcall *transaction-rollback-hooks*)
          (when (< **aborts** most-positive-fixnum)
            (setf **aborts** (1+ **aborts**)))
          (ecase abort-condition
            ((nil))
            ((t) (setf must-acquire t))))))))

(defmacro with-transaction (&body body)
  `(flet ((.transaction-body. ()
            ,@body))
     (call-with-transaction #'.transaction-body.)))

#|
(require 'asdf) (load "stm.asd") (require 'stm) (in-package stm)
(defun foo (n)
  (declare (inline call-with-transaction))
  (loop for i below n
        do (call-with-transaction
            (lambda ()
              (with-transaction-data
                (let ((x (cell-value *cell*)))
                  (loop repeat 1024)
                  (when (zerop (mod i 16))
                    (setf (cell-value *cell*) (1+ x)))))))))
(defun bar (cell n)
  (declare (inline call-with-transaction))
  (loop for i below n
        do (call-with-transaction
            (lambda ()
              (with-transaction-data
                (let ((x (cell-value cell)))
                  (loop repeat 1024)
                  (when (zerop (mod i 4))
                    (setf (cell-value cell) (1+ x)))))))))
(defun baz (n)
  (declare (inline call-with-transaction))
  (loop for i below n
        do (call-with-transaction
            (lambda ()
              (with-transaction-data)))))
(defparameter *cell* (make-cell 0 :rw))
(setf **aborts** 0)
(let ((threads (map-into (make-array 22)
                         (lambda ()
                           (sb-thread:make-thread
                            (lambda ()
                              (foo (* 1024 1024))))))))
  (time (map nil #'sb-thread:join-thread threads))
  (values *cell* **aborts**))
|#
