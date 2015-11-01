(defun divide (num denom)
  (when (zerop denom)
    (error "Sorry, you can't divide by zero."))
  (/ num denom))

;;; define error hierarchy
(define-condition whats-wrong (error)
  ((what :initarg :what :initform "something" :reader what))
  (:report (lambda (condition stream)
             (format stream "Foo! ~@(~A~) is wrong."
                     (what condition))))
  (:documentation "Tell the user that something is wrong."))

(define-condition whats-wrong-and-why (whats-wrong)
  ((why :initarg :why :initform "no clue" :reader why))
  (:report (lambda (condition stream)
             (format stream "Uh oh! ~@(~A~) is wrong. Why? ~@(~A~)."
                     (what condition)
                     (why condition)))))

(define-condition whats-wrong-is-unfathomable (whats-wrong-and-why)
  ()
  (:report (lambda (condition stream)
             (format stream "Gack! ~@(~A~) is wrong for some inexplicable reason."
                     (what condition)))))


;;; assert is excellent for debugging
(defun divide2 (num denom)
  (assert (not (zerop denom)) (num denom)
          ;; optional
          "You can't divide ~d by ~d." num denom)
  (/ num denom))


;;; use handler-bind and signal
;;; example that report on disk space availability.
(define-condition high-disk-utilization ()
  ((disk-name :initarg :disk-name :reader disk-name)
   (current :initarg :current :reader current-utilization)
   (threshold :initarg :threshold :reader threshold))
  (:report (lambda (condition stream)
             (format stream "Disk ~a is ~d% full; threshold is ~d%."
                     (disk-name condition)
                     (current-utilization condition)
                     (threshold condition)))))

(defun get-disk-utilization (disk-name)
  ;; for this example, we'll just return a fixed value
  93)

(defun check-disk-utilization (disk-name threshold)
  (let ((utilization (get-disk-utilization disk-name)))
    (when (>= utilization threshold)
      (signal 'high-disk-utilization
              :disk-name disk-name
              :current utilization
              :threshold threshold))))

(defun log-to-disk (record disk-name)
  (handler-bind ((high-disk-utilization
                  #'(lambda (c)
                      (when (y-or-n-p "~&~A Panic?" c)
                        (return-from log-to-disk nil)))))
    (check-disk-utilization disk-name 90)
    ;; logging
    (print record))
  t)

;;; use cerror
(define-condition expect-type-error (error)
  ((object :initarg :object :reader object)
   (type :initarg :type :reader type))
  (:report (lambda (condition stream)
             (format stream "~S is not of the expected type ~S."
                     (object condition)
                     (type condition)))))

(defun expect-type (object type default-value)
  (if (typep object type)
      object
      (progn
        (cerror "Substitute the default value ~5*~S."  ;choice '0'
                'expect-type-error      ; error class
                :object object          ; rest part, arguments for create the error object as well as for the format string.
                :type type
                :ignore default-value
                :allow-other-keys t)
        default-value)))


;;; good use of restart-bind and return-from
(define-condition device-unresponsive ()
  ((device :initarg :device :reader device))
  (:report (lambda (condition stream)
             (format stream "Device ~A is unresponsive."
                     (device condition)))))

(defun send-query (device query)
  (format t "~&Sending ~S ~S~%" device query))

(defun accept-response (device)
  ;; For the example, the device always fails. 
  nil)

(defun reset-device (device)
  (format t "~&Resetting ~S~%" device))

(defun query-device (device)
  (restart-bind ((nil #'(lambda () (reset-device device))
                      :report-function
                      #'(lambda (stream)
                          (format stream "Reset device.")))
                 (nil #'(lambda ()
                          (format t "~&New device: ")
                          (finish-output)
                          (setq device (read)))
                      :report-function
                      #'(lambda (stream)
                          (format stream "Try a different device.")))
                 (nil #'(lambda ()
                          (return-from query-device :gave-up))
                      :report-function
                      #'(lambda (stream)
                          (format stream "Give up."))))
                (loop
                    (send-query device 'query)
                    (let ((answer (accept-response device)))
                      (if answer
                          (return answer)
                          (cerror "Try again."
                                  'device-unresponsive :device device))))))
; QUERY-DEVICE
; ? (query-device 'foo)
; Sending FOO QUERY
; Error: Device FOO is unresponsive.
; Restart options:
;   1. Try again.
;   2. Reset device.
;   3. Try a different device.
;   4. Give up.
;   5. Top level
;   ? 1
; Sending FOO QUERY
; Error: Device FOO is unresponsive.
; Restart options:
;   1. Try again.
;   2. Reset device.
;   3. Try a different device.
;   4. Give up.
;   5. Top level
;   ? 2
; Resetting FOO
; Restart options:
;   1. Try again.
;   2. Reset device.
;   3. Try a different device.
;   4. Give up.
;   5. Top level
;   ? 1
; Sending FOO QUERY
; Error: Device FOO is unresponsive.
; Restart options:
;   1. Try again.
;   2. Reset device.
;   3. Try a different device.
;   4. Give up.
;   5. Top level
;   ? 3
; New device: bar 
; Restart options:
;   1. Try again.
;   2. Reset device.
;   3. Try a different device.
;   4. Give up.
;   5. Top level
;   ? 1
; Error: Device BAR is unresponsive.
; Restart options:
;   1. Try again.
;   2. Reset device.
;   3. Try a different device.
;   4. Give up.
;   5. Top level
;   ? 4
; :GAVE-UP


;;; good use of ignore errors
(defmacro report-error (&body body)
  (let ((results (gensym))
        (condition (gensym)))
    `(let ((,results (multiple-value-list
                         (ignore-errors 
                           ,@body))))
      (if (and (null (first ,results))
               (typep (second ,results) 'condition)
               (null (nthcdr 2 ,results)))
          (let ((,condition (second ,results)))
            (typecase ,condition
              (simple-condition
               (apply #'format t 
                      (simple-condition-format-control ,condition) 
                      (simple-condition-format-arguments ,condition)))
              (otherwise
               (format t "~A error." (type-of ,condition))))
            (values))
          (values-list ,results)))))
