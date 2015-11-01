;;;; Draft of the clone of the simple example in the presentation of PLT Scheme Simulation Collection by Dr. Williams.
;;; A discrete-event based simulation

;:*=======================
;:* event event-list
;;; An event represents the execution of a task in the future at a specific time.
(defstruct event
  "an event contains: 1. time the event is to occur; 
  				      2.  a task to be executed;
					  3. arguments for the task."
  time task argument)

(defparameter *event-list* '()
  "it is maintained in ascending order
that the first item of the list is the next event to be executed. ")

(defun event-schedule (event event-list)
  "Add an event to the event list and return an event-list with time in ascending order."
  (cond ((null event-list)
         (list event))
        ((< (event-time event)
            (event-time (car event-list)))
         (cons event event-list))
        (t
         (cons (car event-list)
               (event-schedule event (cdr event-list))))))

(defun schedule (event)
  "Add an event to the *event-list*."
  (setf *event-list* (event-schedule event *event-list*)))


;:*=======================
;:* simulation control routine

;;; Global simulation control variables
(defparameter *running?* t "Is the simulation supposed to be running?")
(defparameter *time* 0.0 "current time of the simulation")
(defparameter *time-step* 0.1 "the time difference between current and next simulation")
(defparameter *event* nil "currently executing event")

;;; Not yet decided, see impl below
;; (defun wait (delay)
;;   "..."
;;   )

(defun step-time-forward ()
  (incf *time* *time-step*))

(defun stop-simulation ()
  (setf *running?* nil))

;;; if the *event-list* is null, the simulation will stop
;;; you can add an event to stop simulation at a given point of time.
(defun start-simulation ()
  (loop when *running?* do
        (cond ((null *event-list*)
               (return 'DONE))
              ((<= (event-time (first *event-list*))
                   *time*)
               (progn
                 (setf *event* (first *event-list*))
                 (setf *event-list* (rest *event-list*))
                 (apply (event-task *event*)
                        (event-argument *event*))))
              (t (step-time-forward)))))

;:*=======================
;:* Ramdom Distributions
(defun random-test (fn &rest arg-list)
  "Test function of random distributions. Return the average result "
  (let ((result 0))
    (dotimes (i 100000)
      (incf result (apply fn arg-list)))
    (/ result 100000.0)))

(defun random-float ()
  "Return a random real in (0.0, 1.0]"
  (/ (random most-positive-double-float)
     most-positive-double-float))

(defun random-flat (a b)
  "random-flat: (real real) --> real
Returns a random real number with a uniform distribution between a and b."
  (+ a (* (random-float)
          (- b a))))

(defun random-exp (mean)
  "random-exp: real --> real
Returns a random real number with a exponential distribution with mean"
  (* (- mean)
     (log (random-float))))


;:*=======================
;:* example: Queue modeling
;;; Note: Because lack of real continuations in CL, we have to use recursive style to put all the delayed jobs into the 'wait' macro
(defun generator (n)
  "Generate n customers arriving into the system.
The lambda of the arrival rate is 0.4.
The distribution of arrival time is exponential."
  (when (not (= n 0))
    (wait (random-exp 4.0)
          do (progn						; do something: is delayed computation
               (customer n)
               (generator (- n 1))))))

(defun customer (i)
  "The ith customer into the system.
The customer will stay 2 to 10 minutes with uniform distribution in time."
  (format t "~a: customer ~a enters.~%" *time* i)
  (wait (random-flat 2 10)
        do (format t "~a: customer ~a leaves.~%" *time* i)))

;;; we can now decide the impl of wait.
(defmacro wait (minutes do rest-of-jobs)
  "Wait for several minutes, and do the rest of the jobs.
The scheduling is implemented as an event. And we wrapped the 'continuation'
into a closure which is the function member in the event. 
Finally, we add the event to the event list."
  (if (not (eql do 'do))
      (error "Syntax error: it should be (wait minutes do rest-of-jobs)"))
  `(schedule (make-event :time (+ *time* ,minutes)
						 :task (lambda () ,rest-of-jobs)
						 :argument '())))

;:*=======================
;:* continue our example
(defun run-simulation (n)
  "Run the simulation for n customers 
\(until all the customers have left or until stopped explicitly.)"
  ;; initialize
  (setf *time* 0.0)
  (setf *event-list* '())
  ;; schedule the customer generator
  (schedule (make-event :time 0.0 :task #'generator :argument (list n)))
  (start-simulation))

