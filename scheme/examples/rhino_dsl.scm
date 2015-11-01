;; Placeholder functions
(define (website-alive? url) #f)
(define (notify email-address msg)
  (newline)
  (display "Email sent to: ")
  (display email-address) (newline)
  (display "---") (newline)
  (display msg) (newline)
  (display "---") (newline))

;; Shortcut words for time
(begin
  (define second 'second)
  (define seconds 'seconds)
  (define minute 'minute)
  (define minutes 'minutes)
  (define hour 'hour)
  (define hours 'hours))

;; Convert the shortcut words into seconds for use with the ``sleep'' procedure
(define (time->seconds n unit)
  (case unit
    ((second seconds) n)
    ((minute minutes) (* 60 n))
    ((hour hours) (* 60 60 n))))

;; Syntax available:
;; `(task ,task-name every ,time-value ,time-unit starting now when ,test then ,@do-stuff)
;; `(task ,task-name every ,time-value ,time-unit starting in ,start-time ,start-unit
;;        when ,test then ,@do-stuff)

(define-syntax task
  (syntax-rules (every starting when now then in)
    ((task task-name every time-value time-unit starting in start-value start-unit
	   when test then do-stuff ...)
     ;; The first thread waits
     (thread (lambda ()
	       (sleep (time->seconds start-value start-unit))
	       (thread
		(lambda () (let loop ()
			     (if test
				 (begin do-stuff ...))
			     (sleep (time->seconds time-value time-unit))
			     (loop)))))))
    ((task task-name every time-value time-unit starting now
	   when test then do-stuff ...)
     (thread (lambda () (let loop ()
			  (if test
			      (begin do-stuff ...))
			  (sleep (time->seconds time-value time-unit))
			  (loop)))))))

;; Example task
(task "warn if website is not alive"
      every 3 seconds
      starting in 5 seconds
      when (not (website-alive? "http://example.org"))
      then (notify "admin@example.org" "server down!"))
