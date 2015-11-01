(defpackage :clos-stream
  (:nicknames #:cs)
  (:use #:cl)
  ;; (:shadowing-import-from #:cl
;; 						  #:input-stream-p #:output-stream-p
;; 						  #:force-output #:finish-output
;; 						  #:close #:stream-element-type
;; 						  #:read-char #:write-char
;; 						  #:read-byte #:write-byte)
  )

(in-package :clos-stream)

;;; the class stream

;;; this basic class must be included in all streams.
(defclass stream ()
  ((state :initform 'open :accessor stream-state))
  (:documentation "Foundation of all streams."))

;;; EXTERNAL PROTOCAL supported by all streams

(defgeneric input-stream-p (stream)
  ;; Input streams should override this default method.
  (:method ((stream stream)) nil))

(defgeneric output-stream-p (stream)
  ;; output streams should override this default method.
  (:method ((stream stream)) nil))

(defgeneric close (stream &key abort)
  (:documentation "Prevents further I/O operations on stream")
  (:method ((stream stream) &key abort)
	(declare (ignore abort))
	(setf (stream-state stream) 'closed)))

(defgeneric stream-element-type (stream)
  (:documentation "Returns the type of elements of stream"))

;;; INTERNAL PROTOCAL supported by all streams
(defun ensure-open-stream (stream)
  "Prevents access to a stream if it is not open."
  (let ((state (stream-state stream)))
	(unless (eq state 'open)
	  (error "Attempt to use stream ~A which is ~A"
			 stream state))))

(defgeneric bytes-per-element (stream)
  (declare (values n-bytes))
  (:documentation "Returns length of one element, in 8-bit bytes."))

