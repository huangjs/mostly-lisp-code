;;; a tcp socket library 
;;; in sbcl

(defun resolve-hostname (name)
  "(string)url -> (vector)dotaddress"
  (typecase name
    (string
     (car (sb-bsd-sockets:host-ent-addresses
	   (sb-bsd-sockets:get-host-by-name name))))
    (vector name)))

(defun port (socket)
  "Return the associated port of the socket"
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defun create-listening-socket (host port &optional (max-pending-connection 5))
  "Create a listening socket waiting for the incoming connection."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (resolve-hostname host) port)
    (sb-bsd-sockets:socket-listen socket max-pending-connection)
    socket))

(defun socket-alive? (socket)
  (sb-bsd-sockets:socket-open-p socket))

(defun close-socket (socket)
  (sb-bsd-sockets:socket-close socket))

(defun connect-to-host (host port &key
			(element-type :default)
			(external-format :default)
			(buffering :full))
  "Create a socket to the host:port, and establish the connection.
Return the connection stream."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocal :tcp)))
    (sb-bsd-sockets:socket-connect socket (resolve-hostname host) port)
    (make-socket-stream socket
			:input t
			:output t
			:element-type element-type
			:external-format external-format
			:buffering buffering)))

(defun accept-connection (socket &key
			  (element-type :default)
			  (external-format :default)
			  (buffering :full)) 
  "Return a stream binded to a new socket that is connected to the client"
  (make-socket-stream (sb-bsd-sockets:socket-accept socket) element-type external-format buffering))

(defun make-socket-stream (socket element-type external-format buffering)
  "make a stream on a socket. the socket must have established its connection."
  (sb-bsd-sockets:socket-make-stream socket
				     :output t
				     :input t
				     :element-type element-type
				     :external-format external-format
				     :buffering buffering))

;;; a simple tcp server
(defun print-stream (s &rest args)
  (declare (ignore args))
  (loop for line = (read-line s nil nil)
     while line do (format t "~&~A~%" line)))

(defun create-server (host port
		      &key (allow-multi-conn? nil)
		      (element-type :default)
		      (external-format :default)
		      (buffering :full)
		      (handler #'print-stream)
		      (args nil))
  "Start a TCP server on PORT. If ALLOW-MULTI-CONN is true then the listening 
socket will accept multiple connections, otherwise it will be closed after the 
first incoming connection. A handler is a function that accepts a socket stream as its first input and handler's args as rest."
  (declare (type function handler))
  (let ((socket (create-listening-socket host port)))
    ;; add thread
    (loop do
       ;; add ignore-errors, maybe
	 (let ((connection (accept-connection socket
					      :element-type element-type
					      :external-format external-format
					      :buffering buffering)))
	   ;; add thread
	   (unwind-protect
		(apply handler (cons connection args))
	     (close connection)))
	 while allow-multi-conn?)
    socket))

(defun stop-server (server-socket)
  (when (sb-bsd-sockets:socket-open-p server-socket)
    (sb-bsd-sockets:socket-close server-socket)))

;;; a tcp tunnel
(defun tcp-tunnel-handler (in-stream tar-host tar-port
			   &key (element-type :default)
			   (external-format :default)
			   (debug-stream nil))
  "Read the value from the stream and send it to target host when complete, and receive the value form the host and send it back to the stream. "
  (let ((connection (connect-to-host tar-host tar-port
				     :external-format external-format
				     :element-type element-type)))						
    (declare (type stream connection)
	     (type stream in-stream)
	     (type stream debug-stream))
    (unwind-protect
	 (let ((data nil)
	       (length nil))
	   (loop do
		(format debug-stream "~%~%#<Start of tunneling...>~%")
		(format debug-stream "~%~%#<Sending data to remote host.>~%")
		(setf length (read-sequence data in-stream))
		(when (> length 0)
		  (progn
		    (format connection "~&~A~%" data)
		    (format debug-stream "~&~A~%" data)))
		(format t "~%~%#<Receiving data from remote host.>~%")			  
		(setf length (read-sequence data in-stream))
		(when (> length 0)
		  (progn
		    (format in-stream "~&~A~%" data)
		    (format debug-stream "~&~A~%" data)))))
      (close connection))))

