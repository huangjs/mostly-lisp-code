(defparameter *event-list* nil)

(defstruct event (time function arguments))

(defun schedule (event)
  (setf *event-list* (event-schedule event *event-list*)))

(defun event-schedule (event event-list)
  (cond ((null event-list)
         (list event))
        ((< (event-time event)
            (event-time (car event-list)))
         (cons event event-list))
        (t
         (cons (car event-list)
               (event-schedule event (cdr event-list))))))

(defparameter *time* 0.0)
(defparameter *event* fail)
(defparameter *loop-exit* fail)
(defparameter *loop-next* fail)

(defun (wait/work delay)
    
