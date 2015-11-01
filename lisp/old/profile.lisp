(defun profile1 (fn-name)
  "Make the function count how often it is called"
  ;; First save away the old, unprofiled function
  ;; Then make the name be a new function that increments
  ;; a counter and then calls the original function
  (let ((fn (symbol-function fn-name)))
    (setf (get fn-name 'unprofiled-fn) fn)
    (setf (get fn-name 'profile-count) 0)
    (setf (symbol-function fn-name)
          (profiled-fn fn-name fn))
    fn-name))

(defun unprofile1 (fn-name)
  "Make the function stop counting how often it is called."
  (setf (symbol-function fn-name) (get fn-name 'unprofiled-fn))
  fn-name)

(defun profiled-fn (fn-name fn)
  "Return a function that increments the count."
  #'(lambda (&rest args)
      (incf (get fn-name 'profile-count))
      (apply fn args)))

(defun profile-count (fn-name)
  (get fn-name 'profile-count))

(defun profile-report (fn-names &optional (key #'profile-count))
  "Report profiling statistics on given functions."
  (loop for name in (sort fn-names #'> :key key) do
        (format t "~&~7D ~A" (profile-count name) name)))


;;; Two way to impove it:
;;; 1. it would be nice to have macros that like TRACT and UNTRACE,
;;;    allow the user to profile multiple functions at once and keep track of what has been profiled.
;;; 2. to see the length of time spent in each function could be helpful.

;;; This version has problems:
;;; 1. profiling a function twice is not avoided.
;;; 2. if a profiled function is redefined by the user, the profiled version doesn't change.

;;; Solution:
;;; 1. change the definiton of DEFUN, very risky!!!
;;; 2. ensure the next call to profile will reprofile any functions that has been redefined.
;;;    we do this by keeping track of both the original unprofiled function and the profiled function.


(defvar *profiled-functions* nil
  "Function names that are currently profiled")

(defmacro profile (&rest fn-names)
  "Profile fn-names. With no args, list profiled functions."
  `(mapcar #'profile1
    (setf *profiled-functions*
     (union *profiled-functions* ',fn-names))))

(defmacro unprofile (&rest fn-names)
  "Stop profiling fn-names. With no args, stop all profiling!"
  `(progn
    (mapcar #'unprofile1
     ,(if fn-names `',fn-names '*profiled-functions*))  ; `', means `(quote ,fn-names)
    (setf *profiled-functions*
     ,(if (null fn-names)
          nil
          `(set-difference *profiled-functions* ',fn-names)))))

;;; now we need to change profile1 and unprofile to do the additional bookkeeping
(defun profile1 (fn-name)
  "Make the function count how often it is called"
  ;; First save away the old, unprofiled function
  ;; Then make the name be a new function that increments
  ;; a counter and then calls the original function
  (let ((fn (symbol-function fn-name)))
    (unless (eq fn (get fn-name 'profiled-fn))
      (let ((new-fn (profiled-fn fn-name fn)))
        (setf (symbol-function fn-name) new-fn
              (get fn-name 'profiled-fn) new-fn
              (get fn-name 'unprofiled-fn) fn
              (get fn-name 'profile-time) 0
              (get fn-name 'profile-count) 0))))
  fn-name)

(defun unprofile1 (fn-name)
  "Make the function stop counting how often it is called."
  (setf (get fn-name 'profile-time) 0)
  (setf (get fn-name 'profile-count) 0)
  (when (eq (symbol-function fn-name) (get fn-name 'profiled-fn))
    ;; normal case: restore unprofiled version
    (setf (symbol-function fn-name)
          (get fn-name 'unprofiled-fn)))
  fn-name)

;;; Problem of timing
;;; 1. the overhead of profile.
;;; 2. the overhead of GC profile creates.
;;; 3. platform dependent. (in windows, the accuracy maybe very poor)
;;; 4. repeated timing must be avoided in RECURSIVE functions !!!
;;; therefore, only the function longer than 1/10 sec is trustable.

(defun get-fast-time ()
  "Return the elapsed time. This may wrap around;
use FAST-TIME-DIFFERENCE to compare."
  #+Explorer (time:microsecond-time)    ;do this on an Explorer
  #-Explorer (get-internal-real-time))  ;do this on a non-explorer

(defun fast-time-difference (end start)
  "Substract two time points."
  #+Explorer (time:microsecond-time-difference end start)
  #-Explorer (- end start))

(defun fast-time->seconds (time)
  "Convert a fast-time interval into seconds."
  #+Explorer (/ time 1000000.0)
  #-Explorer (/ time internal-time-units-per-second))


;;; now we need to update PROFILED-FN
;;; we charge each function only for the time since the last call or return
(proclaim '(inline profile-enter profile-exit inc-profile-time))

(defun profiled-fn (fn-name fn)
  "Return a function that increments the count, and times."
  #'(lambda (&rest args)
      (profile-enter fn-name)
      (multiple-value-prog1
          (apply fn args)
        (profile-exit fn-name))))

(defvar *profile-call-stack* nil)

(defun profile-enter (fn-name)
  (incf (get fn-name 'profile-count))
  (unless (null *profile-call-stack*)
    ;; Time charged against the calling function:
    (inc-profile-time (first *profile-call-stack*)
                      (car (first *profile-call-stack*))))
  ;; Put a new entry on the stack
  (push (cons fn-name (get-fast-time))
        *profile-call-stack*))

(defun profile-exit (fn-name)
  ;; Time charged against te current function:
  (inc-profile-time (pop *profile-call-stack*)
                    fn-name)
  ;; Change the top entry to reflect current time
  (unless (null *profile-call-stack*)
    (setf (cdr (first *profile-call-stack*))
          (get-fast-time))))

(defun inc-profile-time (entry fn-name)
  (incf (get fn-name 'profile-time)
        (fast-time-difference (get-fast-time) (cdr entry))))

;;; finally we need to update PROFILE-REPORT to print the timing data as well as the counts.
(defun profile-report (&optional
                       (fn-names (copy-list *profiled-functions*))
                       (key #'profile-count))
  "Report profiling statistics on given functions."
  (let ((total-time (reduce #'+ (mapcar #'profile-time fn-names))))
    (unless (null key)
      (setf fn-names (sort fn-names #'> :key key)))
    (format t "~&Total elapsed time: ~d seconds."
            (fast-time->seconds total-time))
    (format t "~&  Count   Secs  Time% Name")
    (loop for name in fn-names do
          (format t "~&~7D ~6,2F ~3D% ~A"
                  (profile-count name)
                  (fast-time->seconds (profile-time name))
                  (round (/ (profile-time name) total-time) 0.1)
                  name))))

(defun profile-time (fn-name)
  (get fn-name 'profile-time))

;;; profile -> profile-report -> unprofile
;;; It's convenient to provide a single macro
(defmacro with-profiling (fn-names &rest body)
  `(progn
    (unprofile . ,fn-names)
    (profile . ,fn-names)
    (setf *profile-call-stack* nil)
    (unwind-protect
         (progn . ,body)
      (profile-report ',fn-names)
      (unprofile . ,fn-names))))
