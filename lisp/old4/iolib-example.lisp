(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel)
  (asdf:oos 'asdf:load-op 'iolib))

(defpackage :iolib-problem
  (:use #:common-lisp #:net.sockets #:iomux))

(in-package :iolib-problem)

(defparameter *server* nil)
(defparameter *event-base* (make-instance 'iomux:event-base))
(defparameter *event-loop-thread* nil)
(defparameter *server-event* nil)

(defun handle-connection (sock handler)
  (unwind-protect
       (progn
	 (funcall handler sock)
	 (finish-output sock))
    (close sock)))

(defun make-server (port)
  (let ((sock (create-socket :address-family
			     :internet
			     :type
			     :stream
			     :connect
			     :passive
			     :ipv6 nil)))
    (bind-address sock +ipv4-unspecified+ :port port)
    (socket-listen sock)
    sock))

(defun add-connection-handler (event-base connection request-handler)
  (let ((handle))
    (labels
	((close-handler ()
	   (remove-event event-base handle)
	   (close connection))
	 (handler (fd evtype)
	   (declare (ignorable fd)) 
	   (handler-case
	       (cond
		 ((eq evtype :read)
		  (funcall request-handler connection)
		  (close-handler))
		 (t (close-handler)))
	     (condition (c)
	       (declare (ignorable c))
	       (describe c)))
	   ))
      (setf handle (iomux:add-fd event-base (sockets::socket-fd connection) :read #'handler :persistent t :timeout 1)))))

(defun add-single-threaded-server (event-base listener-socket request-handler)
  (labels
      ((handler (fd evtype)
	 (declare (ignorable fd))
	 (case evtype
	   (:read (let ((connection (accept-connection listener-socket)))
		    (add-connection-handler event-base connection request-handler)))
	   (:otherwise (break)))))
    (iomux::add-fd event-base (sockets::socket-fd listener-socket)
		   :read #'handler :persistent t :timeout 1)))

(defun start-server (port)
  (setf *server* (make-server port))
  (setf *server-event*
	(add-single-threaded-server *event-base* *server* #'test-handler))
  (event-dispatch *event-base*))

(defun stop-server ()
  (close *server*)
  (remove-event *event-base* *server-event*)
  (setf *server* nil)
  (setf *server-event* nil))

(defparameter *response-body* "<HTML><HEAD><TITLE>Test</TITLE></HEAD><BODY>AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA</BODY></HTML>")
(defparameter *response-header* (format nil "HTTP/1.1 200 OK~%Content-Type: text/html~%Content-Length: ~A~%"
					(length *response-body*)))

(defun test-handler (sock)
  (format sock "~A~%~A" *response-header* *response-body*))
