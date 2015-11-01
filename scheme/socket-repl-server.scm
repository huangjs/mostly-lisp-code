(module socket-repl
  (main main))

(define s1 (make-server-socket)) 
(define s2 #unspecified) 

(define (main argv)
   (dynamic-wind 
      ;; Init: Launch an xterm with telnet running 
      ;; on the s listening port and connect 
      (lambda () 
	 (run-process "/usr/X11R6/bin/xterm" "-display" ":0" "-e" "telnet" "localhost" 
		      (number->string (socket-port-number s1))) 
	 (set! s2 (socket-accept s1)) 
	 (display #"\nWelcome on the socket REPL.\n\n> " (socket-output s2)) 
	 (flush-output-port (socket-output s2))) 
      ;; Action: A toplevel like loop 
      (lambda () 
	 (let loop () 
	    (let ((obj (eval (read (socket-input s2))))) 
	       (fprint (socket-output s2) "; Result: " obj) 
	       (display "> " (socket-output s2)) 
	       (flush-output-port (socket-output s2)) 
	       (loop)))) 
      ;; Termination: We go here when 
      ;; -a: an error occurs 
      ;; -b: connection is closed 
      (lambda () 
	 (print #"Shutdown ......\n") 
	 (socket-close s2) 
	 (socket-shutdown s1)))) 

