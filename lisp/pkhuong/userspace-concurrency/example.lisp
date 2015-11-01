(defpackage "SB-EVENT-EXAMPLE"
    (:use "CL" "SB-EXT" "SB-EVENT"))
(in-package "SB-EVENT-EXAMPLE")

(declaim (inline mod< mod<= mod-min))
(defun mod< (x y width)
  (>= (ldb (byte width 0) (- x y))
      (ash 1 (1- width))))
(defun mod<= (x y width)
  (not (mod< y x width)))
(defun mod-min (x y width)
  (if (mod< x y width) x y))

;;; Unfair spinlock.
(defstruct (spinlock
             (:constructor make-spinlock ()))
  (owner nil)
  (event (make-event) :type event :read-only t))

(defun get-spinlock (lock
                     &aux
                     (event (spinlock-event lock))
                     (token (event-token event))
                     (self sb-thread:*current-thread*))
  (declare (type spinlock lock))
  (loop named outer do
    (loop repeat 256 do
      (when (and (null (spinlock-owner lock))
                 (null (compare-and-swap (spinlock-owner lock)
                                         nil self)))
        (return-from outer))
      (spin-loop-hint))
    (setf token (event-wait event token))))

(defun release-spinlock (lock)
  (declare (type spinlock lock))
  (assert (eql sb-thread:*current-thread* (spinlock-owner lock)))
  (sb-thread:barrier (:memory)
    (setf (spinlock-owner lock) nil))
  (event-signal (spinlock-event lock) 1)
  nil)

;;; Fair condition variable:
;;;  Two counters are packed in a single word, one for the number of
;;;  signals, the other for the wait ticket.
;;;  A thread is woken up whenever the signal count is >= its ticket.
;;;
;;;  Spinning sucks, so, after a while, threads wait on an event.
;;;  Ticket ids are mapped to the events array by:
;;;   Chunks of +waiters-per-event+ threads are mapped to the same
;;;   event (TRUNCATE);  events are addressed modularly in the event
;;;   vector.
(defconstant +waiters-per-event+ 4)
(defconstant +events-per-condvar+ 16)
(defstruct (condvar
             (:constructor make-condvar ()))
  ;; counter, 0:31 are for signalled count, 32:63 are for wait tickets
  (counter 0 :type word)
  (events   (map-into (make-array +events-per-condvar+) #'make-event)
   :type (simple-array event (#.+events-per-condvar+)) :read-only t))
(declaim (freeze-type condvar))
(sb-event::define-word-casser condvar-counter-compare-and-swap condvar counter)

(defun condition-broadcast (condvar)
  (declare (type condvar condvar))
  (loop
   (let* ((counter     (condvar-counter condvar))
          (epoch       (ldb (byte 32 32) counter))
          (new-counter (logior (ash epoch 32) epoch)))
     (when (= counter (condvar-counter-compare-and-swap
                       condvar counter new-counter))
       (return))
     (warn "Concurrent access to condition variable ~A" condvar)))
  (map nil (lambda (event)
             (event-broadcast event nil))
       (condvar-events condvar)))

(defun condition-signal (condvar &optional (count 1))
  (declare (type condvar condvar))
  (loop
    (let* ((counter     (condvar-counter condvar))
           (epoch       (ldb (byte 32 32) counter))
           (signalled   (ldb (byte 32 0) counter))
           (new-signalled (mod-min epoch
                                   (mod (+ count signalled)
                                        (ash 1 32))
                                   32))
           (new-counter (logior (ash epoch 32) new-signalled)))
      (when (= counter (condvar-counter-compare-and-swap
                        condvar counter new-counter))
        (let ((start  (mod (truncate epoch +waiters-per-event+)
                           +events-per-condvar+))
              (events (condvar-events condvar)))
          (dotimes (i (min (ceiling count +waiters-per-event+)
                           +events-per-condvar+))
            (event-broadcast (aref events (mod (+ i start)
                                               +events-per-condvar+))
                             nil)))
        (return))
      (warn "Concurrent access to condition variable ~A" condvar))))

(defun condition-wait (condvar spinlock)
  (declare (type condvar condvar)
           (type spinlock spinlock))
  (assert (eql sb-thread:*current-thread* (spinlock-owner spinlock)))
  (let* ((counter (atomic-incf (condvar-counter condvar) (ash 1 32)))
         (epoch   (ldb (byte 32 32) (+ counter (ash 1 32))))
         (event   (aref (condvar-events condvar)
                        (mod (truncate epoch +waiters-per-event+)
                             +events-per-condvar+)))
         token)
    (release-spinlock spinlock)
    (loop named outer do
      (loop repeat 128 do
        (when (mod<= epoch (ldb (byte 32 0) (condvar-counter condvar)) 32)
          (return-from outer))
        (spin-loop-hint))
      (setf token (event-wait event token)))
    (get-spinlock spinlock)))
