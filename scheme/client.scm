(module client) 

(let* ((s (make-client-socket "localhost" 8080 #f)) 
       (p (socket-output s))) 
   (display "string" p) 
   (newline p) 
   (display "abc" p) 
   (flush-output-port p) 
   (let loop () 
      (loop))) 

