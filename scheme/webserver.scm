(module server
	(library pthread)
	(main main))

(define *port-num* 3333)

(define (main argv)
  (let ((socket0 (make-server-socket *port-num*)))
    (thread-join!
     (thread-start-joinable!
      (make-thread (lambda () (start-server socket0)))))
    (fprint (current-error-port) "Shutting down...")))

(define (start-server socket0)
  (print "Starting server...")
  (let loop ()
    (print "waiting for connection")
    (let* ((s (socket-accept socket0 :inbuf 100 :outbuf 100)))
      (print "New connection: " s)
      (thread-start! (make-thread (lambda () (handle-request s)) 'foo))
      (loop))))

(define (handle-request s)
  (define (readline)
    (read-line (socket-input s)))

  (define (read-request)
    (readline))

  (define (read-headers)
    (let loop ((headers (list)))
      (let ((line (readline)))
	(if (string=? line "")
	    headers
	    (loop (cons line headers))))))

  (let* ((req (read-request))
	 (headers (read-headers)))
    (when (string=? req "GET /kill HTTP/1.0")
	  (exit))
    (print "Request: " req "\r\nHeaders: " headers)
    (socket-print s (string-append "HTTP/1.0 200 OK\r\n\r\n" "Hello"))
    (socket-shutdown s #f)))

(define (socket-print s str)
  (with-output-to-port (socket-output s)
    (lambda ()
      (print str)
      (flush-output-port (current-output-port)))))
