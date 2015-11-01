;;;FIXME!!
;;;runs only on SBCL

(defpackage #:zpb
  (:use #:cl)
  (:export #:uptime))

(in-package #:zpb)

(defun uptime-values ()
  (let ((seconds (truncate (get-internal-real-time)
                           internal-time-units-per-second)))
    (multiple-value-bind (days seconds)
        (truncate seconds 86400)
      (multiple-value-bind (hours seconds)
          (truncate seconds 3600)
        (multiple-value-bind (minutes seconds)
            (truncate seconds 60)
          (values days hours minutes seconds))))))

(defun uptime ()
  (multiple-value-bind (days hours minutes seconds)
      (uptime-values)
    (format t "~[~:;~:*~D days ~]~
               ~[~:;~:*~D hours ~]~
               ~[~:;~:*~D minutes ~]~
               ~[~:;~:*~D seconds~]~%"
            days hours minutes seconds)))
