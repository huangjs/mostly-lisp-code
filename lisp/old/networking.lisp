(defun server ()
  (let ((a-server-socket (socket-server 8000)))
    (dotimes (i 2)                      ; accept 2 connections, then quit
      (let* ((connection (socket-accept a-server-socket))
             (line (read-line connection)))
        (format t "Line from client: ~a~%" line)
        ;; send something back to the server:
        (format connection "response from server~%")
        (close connection)))
    (socket-server-close a-server-socket)))
        
