(in-package :user)

(eval-when (compile load eval)
  (require :sock))

(defun start-server (buffer)
  (let ((sock (socket:make-socket :connect :passive)))
    (mp::process-run-function
     "server" #'server-get-connection sock buffer)
    (socket:local-port sock)))

(defun server-get-connection (sock buffer)
  (let ((stm (socket:accept-connection sock)))
    (close sock) ;; don't need it anymore
    (unwind-protect
	 (progn
	   (read-in-buffer stm buffer)
	   (write-out-buffer stm buffer))
      (close stm))
    (break "server breaks")))

(defun run-client (port buffer)
  (let ((stm (socket:make-socket :remote-host "localhost"
				 :remote-port port)))
    (unwind-protect
	 (progn
	   (dotimes (i (length buffer))
	     (setf (aref buffer i) (mod i 256)))
	   (write-out-buffer stm buffer)
	   (socket:shutdown stm :direction :output)
	   (read-in-buffer stm buffer)
	   )
      (close stm))))

(defun write-out-buffer (stm buffer)
  (dotimes (i (length buffer))
    (write-byte (aref buffer i) stm)))

(defun read-in-buffer (stm buffer)
  (dotimes (i (length buffer))
    (setf (aref buffer i)
	  (read-byte stm))))

(defun socket-run (&optional (message-length 10))
  (let ((client-buffer
	 (make-array message-length :element-type '(unsigned-byte 8)))
	(server-buffer
	 (make-array message-length :element-type '(unsigned-byte 8))))
    (run-client (start-server server-buffer) client-buffer)
    (format t "~s~%" client-buffer)
    (force-output)
    (break "client breaks")))
