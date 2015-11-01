;;;; THE CLASS STREAM

(defpackage :clos-streams 
  (:nicknames :cs)
  (:use :cl)
  (:shadow #:input-stream-p
	   #:output-stream-p
	   #:force-output
	   #:finish-output
	   #:close
	   #:stream-element-type
	   #:read-char
	   #:write-char
	   #:read-byte
	   #:write-byte
	   #:stream))

(in-package :clos-streams)

(declaim (declaration values))

;;; This basic class must be included in all streams.
(defclass stream ()
  ((state :initform 'open :accessor stream-state))
  (:documentation "Foundation of all streams."))


;;;; external protocol supported by all streams

;;; Page 332 in CLTL
(defgeneric input-stream-p (stream)
  ;; Input streams should override this default method.
  (:method ((stream stream)) nil))

(defgeneric output-stream-p (stream)
  ;; Output streams should override this default method.
  (:method ((stream stream)) nil))

(defgeneric close (stream &key abort)
  (:documentation "Prevents further I/O operations on stream")
  (:method ((stream stream) &key abort)
    (declare (ignore abort))
    (setf (stream-state stream) 'closed)))

(defgeneric stream-element-type (stream)
  ;; Must be implemented by the element type streams.
  (:documentation "Returns the type of elements of stream"))


;;;; internal protocol supported by all streams

(defun ensure-open-stream (stream)
  "Prevents access to a stream if it is not open."
  (let ((state (stream-state stream)))
    (unless (eq state 'open)
      (error "Attempt to use stream ~A which is ~A"
	     stream state))))

(defgeneric bytes-per-element (stream)
  ;; must be implemented by element type streams.
  (declare (values n-bytes))
  (:documentation "Returns length of one element, in 8-bit bytes."))

(defgeneric storage-unit-size (stream)
  ;; must be implemented by device streams.
  (declare (values n-bytes))
  (:documentation "Return size of i/o buffer, in 8-bit bytes."))

(defun make-element-array (stream)
  "Returns array of correct size and element type for stream."
  (make-array (/ (storage-unit-size stream)
		 (bytes-per-element stream))
	      :element-type (stream-element-type stream)))


;;;; the class input-stream and its methods

;;; this basic class must be included in all input streams.
(defclass input-stream (stream) ()
  (:documentation "Foundation of all input streams."))

;;; Override the default primary method to return true.
(defmethod input-stream-p ((stream input-stream))
  t)


;;;; internal protocol supported by all input streams

;;; make sure the stream is open before any input is allowed.
(defgeneric read-next-element (input-stream)
  (declare (values element eof-p))
  (:method :before ((stream input-stream))
	   ;; this method ensures that stream is open before
	   ;; reading; it is inherited by all element type input
	   ;; streams, so it saves each of those methods from
	   ;; duplicating this code.
	   (ensure-open-stream stream)))

;;; this default method on stream is overridden by input
;;; streams. It is defined simply to give a comprehensible
;;; error message when this situation occurs, and to make it
;;; unnecessary for all external functions to check the
;;; stream argument type.
(defmethod read-next-element ((stream stream))
  (error "Cannot get input from stream ~A of type ~A."
	 stream (type-of stream)))


;;;; the class output-stream and its methods

;;; this basic class must be included in all output streams.
(defclass output-stream (stream) ()
  (:documentation "Foundation of all output streams."))

;;; override the default primary method to return true.
(defmethod output-stream-p ((stream output-stream))
  t)


;;;; external protocol supported by all output streams

;;; although common lisp implies that force-output
;;; and finish-output are supported by character
;;; streams only, they should apply to all output
;;; output streams, which is how we do it below.

;;; also, since common lisp specifies that the stream
;;; argument of force-output and finish-output is
;;; optional, we can't implement these operations
;;; directly as generic functions.
;;; therefore, we define force-output-internal and
;;; finish-output-internal as generic functions, both of
;;; which belong to the internal protocol.

;;; standardize stream variable if t or nil was given
(defmacro standardize-output-stream-var (stream)
  `(setf ,stream (cond ((eq ,stream t) *terminal-io*)
		       ((null ,stream) *standard-output*)
		       (t ,stream))))

(defun force-output (&optional (stream *standard-output*))
  (standardize-output-stream-var stream)
  (force-output-internal stream))


(defun finish-output (&optional (stream *standard-output*))
  (standardize-output-stream-var stream)
  (finish-output-internal stream))


;;;; internal protocol supported by all output streams

(defgeneric force-output-internal (output-stream)
  (:method :before ((stream output-stream))
	   ;; the stream must be open, else generate an error.
	   (ensure-open-stream stream)))

(defgeneric finish-output-internal (output-stream)
  (:method :before ((stream output-stream))
	   ;; the stream must be open, else generate an error.
	   (ensure-open-stream stream)))

(defgeneric write-next-element (output-stream element)
  (:method :before ((stream output-stream) element)
	   (declare (ignore element))
	   ;; default method ensures that stream is open before
	   ;; writing. this method is inherited by all element type
	   ;; output streams, and thus it saves each of those
	   ;; methods from duplicating this code.
	   (ensure-open-stream stream)))


;;; this default method on stream is overridden by output streams.
;;; it is defined simply to give a comprehensible error message
;;; when this situation occurs, and to make it unnecessary for
;;; all user-interface functions to check the stream argument type
(defmethod write-next-element ((stream stream) element)
  (declare (ignore element))
  (error "Cannot do output to stream ~A of type ~A."
	 stream (type-of stream)))


;;;; the class bidirectional-stream

;;; this class inherits all needed methods, and supplies none
;;; of its own.
(defclass bidirectional-stream (input-stream output-stream) ()
  (:documentation "A combined input and output stream."))

