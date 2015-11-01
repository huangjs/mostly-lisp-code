(defpackage :hjs.utils.time
  (:use :cl)
  (:export #:display-readable-time))

(in-package :hjs.utils.time)

(defvar *day-names*
  '("Monday" "Tuesday" "Wednesday"
	"Thursday" "Friday" "Saturday"
	"Sunday"))

(defun display-readable-time ()
  (multiple-value-bind
		(second minute hour date month year day-of-week dst-p tz)
	  (get-decoded-time)
	(format t "It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
			hour
			minute
			second
			(nth day-of-week *day-names*)
			month
			date
			year
			(- tz))))

