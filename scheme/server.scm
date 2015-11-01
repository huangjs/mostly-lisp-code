(module server) 

(let* ((s (make-server-socket 8080)) 
       (s2 (socket-accept s #f))) 
   (let ((pin (socket-input s2))) 
      (let loop () 
	 (display (read-char pin)) 
	 (flush-output-port (current-output-port)) 
	 (loop)))) 
