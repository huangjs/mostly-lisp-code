;;; user space concurrency control.
;;; When you can't trust pthreads, hopefully poll/read will be more solid.
;;;
;;; Events are a sane alternative to condition variables:
;;; - Condition variables let you wait until another thread signals.
;;;
;;; - Events let you wait until another thread has signalled since the last
;;; time the event has been waited on.  i.e., no need for a mutex to protect
;;; against stupid race conditions.
;;;
;;; The downside: events are really best-suited to having a single waiter.
;;;
;;; API:
;;;
;;; MAKE-EVENT => event
;;;  Return a fresh event object of type EVENT (check with EVENT-P).
;;;  There is no clean-event or some such.  Getting it right with
;;;  concurrency can be hard, so we just rely on the GC and finalizers.
;;
;;; SIGNAL-EVENT event => boolean
;;;  Signal the event.  Returns whether this is the first signal to the
;;;  event since its creation or last call to CLEAR-EVENT
;;;
;;; CLEAR-EVENT event => NIL, or non-negative integer
;;;  Clear the event flag.  Returns NIL if no SIGNAL has happened since
;;;  the object creation/last reset, number of signals otherwise.
;;;
;;; PEEK-EVENT event timeout => NIL or non-negative integer
;;;  Wait for up to timeout milliseconds (infinite if NIL) while the event
;;;  has not been signalled since the last CLEAR.
;;;  Returns NIL if timed out, number of signals otherwise (can be zero if
;;;  returns due to interrupts!)
;;;
;;; CHECK-EVENT event timeout => NIL or non-negative integer
;;;  PEEK-EVENT, followed by CLEAR-EVENT
;;;
;;; These are meant as an optimisation on top of concurrent data structures that
;;; use backoff/sleep.
;;; See bottom of file for an example.

(defpackage "SB-EVENT"
    (:use "CL" "SB-EXT" "SB-ALIEN")
  (:shadow "READ" "WRITE")
  (:export "EVENT" "MAKE-EVENT" "EVENT-P"
           "SIGNAL-EVENT" "CLEAR-EVENT" "PEEK-EVENT" "CHECK-EVENT"
           "MAYBE-SIGNAL-EVENT" "MAYBE-PEEK-OR-CHECK-EVENT"
           "ENSURE-EVENT" "MAYBE-CLEAR-EVENT"
           "ENSURE-EVENT-AND-AWAIT"))
(in-package "SB-EVENT")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (handler-bind ((simple-error (lambda (condition)
                                 (declare (ignore condition))
                                 (invoke-restart 'continue))))
    (sb-c:defknown spin-hint () (values))
    (sb-c:defknown word-compare-and-swap (system-area-pointer word word) word))
  
  (sb-c:define-vop (spin-hint)
    (:translate spin-hint)
    (:policy :fast-safe)
    (:generator 5
      (sb-assem:inst sb-vm::pause)))

  (sb-c:define-vop (word-compare-and-swap)
    (:translate word-compare-and-swap)
    (:policy :fast-safe)
    (:args (sap :scs (sb-vm::sap-reg) :to :eval)
           (old :scs (sb-vm::unsigned-reg) :target eax)
           (new :scs (sb-vm::unsigned-reg)))
    (:arg-types system-area-pointer sb-vm::unsigned-num sb-vm::unsigned-num)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rax-offset
                     :from (:argument 1) :to :result :target r)
                eax)
    (:results (r :scs (sb-vm::unsigned-reg)))
    (:result-types sb-vm::unsigned-num)
    (:generator 10
      (sb-vm::move eax old)
      (sb-vm::inst sb-vm::cmpxchg (sb-vm::make-ea :qword :base sap) new :lock)
      (sb-vm::move r eax))))

(defun spin-hint ()
  (spin-hint))

(defun word-compare-and-swap (sap old new)
  (declare (type system-area-pointer sap)
           (word old new))
  (word-compare-and-swap sap old new))

(define-alien-type poll-fd
    (struct poll-fd
            (fd int)
            (events short)
            (revents short)))

(declaim (inline poll))
(define-alien-routine poll int
  (fds (* poll-fd))
  (nfds unsigned)
  (timeout int))

(declaim (inline read write))
(define-alien-routine read int
  (fd int)
  (buf (* t))
  (nbytes unsigned-long))

(define-alien-routine write int
  (fd int) (buf (* t)) (nbytes unsigned-long))

(defun strerror (&optional (errno (get-errno)))
  (alien-funcall (extern-alien "strerror" (function c-string int)) errno))

(defun get-pipe ()
  (sb-posix:pipe))

(defun set-nonblocking (fd)
  (sb-posix:fcntl fd sb-posix:f-setfl sb-posix:o-nonblock))

(defun get-event-pipe ()
  (multiple-value-bind (read write)
      (get-pipe)
    (set-nonblocking write)
    (set-nonblocking read)
    (values read write)))

;;; Event is a counter (state), and a non-blocking pipe for
;;; the slow paths.
;;;
;;; The counter counts the number of signals since the last reset
;;; in bits 1:63.  The least significant bit is a flag to denote
;;; upgrades to the slow path: when 1, at least one waiter has entered
;;; the slow path.
;;;
;;; When upgraded to the slow path, signal must also write to the
;;; signal FD.
(defstruct (event
             (:constructor %make-event (signal check)))
  (signal 0 :type (signed-byte 32) :read-only t)
  (check  0 :type (signed-byte 32) :read-only t)
  (state  0 :type sb-vm:word))
(declaim (freeze-type event))

(sb-kernel:define-structure-slot-addressor event-state-sap :structure event :slot state)
(declaim (inline event-state-compare-and-swap))
(defun event-state-compare-and-swap (event old new)
  (declare (type event event)
           (type word old new))
  (sb-sys:with-pinned-objects (event)
    (word-compare-and-swap (sb-sys:int-sap (event-state-sap event))
                           old new)))

(defun make-event ()
  (multiple-value-bind (read write)
      (get-event-pipe)
    (let ((event (%make-event write read)))
      (finalize event (lambda ()
                        (sb-posix:close read)
                        (sb-posix:close write))))))

(declaim (type (simple-array (unsigned-byte 8) (4096)) **buffer**))
(defglobal **buffer** (make-array 4096 :element-type '(unsigned-byte 8)))

(defun %signal-event (event)
  (declare (type event event)
           (optimize speed))
  (let ((buffer (load-time-value **buffer** t)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((err (write (event-signal event)
                        (sb-sys:vector-sap buffer)
                        1)))
        (when (and (minusp err)
                   (/= err (- sb-posix:eagain)))
          (error "Error in ~S: ~A (~A)" 'signal-event (strerror (- err)) (- err)))))))

(declaim (maybe-inline signal-event clear-event))
(defun signal-event (event)
  (declare (type event event)
           (optimize speed))
  (let ((old-value (atomic-incf (event-state event) 2)))
    (when (oddp old-value)
      (%signal-event event))
    (<= old-value 1)))

(defun %clear-event (event)
  (declare (type event event))
  (let ((buffer (load-time-value **buffer** t))
        (fd     (event-check event)))
    (sb-sys:with-pinned-objects (buffer)
      (setf (aref buffer 0) 0) ; clear any WP
      (let ((err (read fd (sb-sys:vector-sap buffer) 4096)))
        (when (and (minusp err)
                   (/= (get-errno) sb-posix:eagain))
          (error "Error in ~S: ~A"
                 'clear-event (strerror)))))))

(defun clear-event (event)
  (declare (type event event))
  (let ((state (event-state event)))
    (when (oddp (event-state event))
      (%clear-event event))
    (sb-thread:barrier (:write)
      (setf (event-state event) 0))
    (and (> state 1)
         (min (ash state -1) most-positive-fixnum))))

(defun %peek-event (event timeout)
  "milliseconds timeout or nil... nil is a bad idea."
  (declare (type event event)
           (type (or null (unsigned-byte 31)) timeout)
           (optimize speed))
  (let ((fd     (event-check event)))
    (with-alien ((poll-fd poll-fd))
      (setf (slot poll-fd 'fd)     fd
            (slot poll-fd 'events) sb-unix::pollin)
      (ecase (poll (addr poll-fd) 1 (or timeout -1))
        ((-1 1) (min (ash (event-state event) -1)
                     most-positive-fixnum))
        ;; timeout
        (0  nil)))))

(defun peek-event (event timeout)
  (declare (type event event)
           (type (or null (unsigned-byte 31)) timeout)
           (optimize speed))
  (when (zerop (event-state event))
    (loop repeat 128
          do
       (let ((state (event-state event)))
         (when (> state 1)
           (return-from peek-event (min (ash state -1)
                                        most-positive-fixnum))))
       (spin-hint)))
  (let ((old (event-state event)))
    (when (zerop old)
      (setf old (event-state-compare-and-swap event 0 1)))
    (if (<= old 1)
        (%peek-event event timeout)
        (min most-positive-fixnum (ash old -1)))))

(declaim (maybe-inline check-event))
(defun check-event (event timeout)
  (declare (type event event)
           (type (or null (unsigned-byte 32)) timeout)
           (inline clear-event))
  (peek-event event timeout)
  (clear-event event))

(declaim (maybe-inline maybe-peek-or-check-event maybe-signal-event))
(defun maybe-peek-or-check-event (event self timeout)
  (declare (type event event self)
           (type (or null (unsigned-byte 32)) timeout))
  (cond ((null event))
        ((eql event self)
         (check-event self timeout))
        (t
         (peek-event event timeout))))

(defun maybe-signal-event (event)
  (declare (type (or null event) event)
           (inline signal-event))
  (when event
    (signal-event event)))

(defmacro ensure-event (place self &environment env)
  (multiple-value-bind (vars vals store writer reader)
      (get-setf-expansion place env)
    (assert (= 1 (length store)))
    (let ((_self    (gensym "SELF")))
      `(let* ((,_self    ,self)
              ,@(mapcar  #'list vars vals))
         (or ,reader
             (let ((,(first store) ,_self))
               ,writer
               ,_self))))))

(defmacro maybe-clear-event (place self &environment env)
  (multiple-value-bind (vars vals store writer reader)
      (get-setf-expansion place env)
    (assert (= 1 (length store)))
    (let ((_self    (gensym "SELF")))
      `(let* ((,_self    ,self)
              ,@(mapcar  #'list vars vals))
         (when (eql ,reader ,_self)
           (let ((,(first store) nil))
             ,writer)
           t)))))

(defmacro ensure-event-and-await (place self timeout)
  (let ((_self    (gensym "SELF"))
        (_timeout (gensym "TIMEOUT")))
    `(let ((,_self    ,self)
           (,_timeout ,timeout))
       (maybe-peek-or-check-event (ensure-event ,place ,_self)
                                  ,_self ,_timeout))))

;; Example: A sleep-ful lock.
;;
;; Start with a regular spinlock.
;; Add a hint to sleep on.
;; Instead of sleeping, threads either get the current hint or set it to our
;; own event.
;; No need for atomic operations on the hint: it's optimistic, and any wait
;; is bounded.

(defstruct (spinlock
             (:constructor make-spinlock ()))
  (owner nil :type (or null event))
  (hint  nil :type (or null event)))

(defun get-spinlock (lock event)
  (declare (type spinlock lock)
           (type event event))
  (loop named outer do
    (loop repeat 256 do
      (when (and (null (spinlock-owner lock))
                 (null (compare-and-swap (spinlock-owner lock)
                                         nil event)))
        (return-from outer))
      (spin-hint))
    (ensure-event-and-await (spinlock-hint lock) event 100))
  (when (eql (spinlock-hint lock) event)
    (setf (spinlock-hint lock) nil))
  nil)

(defun release-spinlock (lock)
  (declare (type spinlock lock)
           (inline signal-event))
  (sb-thread:barrier (:memory)
    (setf (spinlock-owner lock) nil))
  (let ((hint (spinlock-hint lock)))
    (when hint
      (signal-event hint)))
  nil)

(defstruct (condvar
             (:constructor make-condvar ()))
  ;; counter, 0:31 are for signalled count, 32:63 are for wait tickets
  (counter 0 :type word)
  (hint nil :type (or null event)))
(declaim (freeze-type condvar))

(sb-kernel:define-structure-slot-addressor condvar-counter-sap :structure condvar :slot counter)
(declaim (inline condvar-counter-compare-and-swap))
(defun condvar-counter-compare-and-swap (condvar old new)
  (declare (type condvar condvar)
           (type word old new))
  (sb-sys:with-pinned-objects (condvar)
    (word-compare-and-swap (sb-sys:int-sap (condvar-counter-sap condvar))
                           old new)))

(declaim (inline mod<= mod-min))
(defun mod<= (x y)
  (< (logand (- y x) most-positive-fixnum) (truncate (1+ most-positive-fixnum) 2)))
(defun mod-min (x y)
  (if (mod<= x y) x y))

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
  (maybe-signal-event (condvar-hint condvar)))

(defun condition-signal (condvar &optional (count 1))
  (declare (type condvar condvar))
  (loop
    (let* ((counter     (condvar-counter condvar))
           (epoch       (ldb (byte 32 32) counter))
           (signalled   (ldb (byte 32 0) counter))
           (new-counter (logior (ash epoch 32)
                                (mod-min epoch
                                         (mod (+ count signalled)
                                              (ash 1 32))))))
      (when (= counter (condvar-counter-compare-and-swap
                        condvar counter new-counter))
        (return))
      (warn "Concurrent access to condition variable ~A" condvar)))
  (maybe-signal-event (condvar-hint condvar)))

(defun condition-wait (condvar spinlock self)
  (declare (type condvar condvar)
           (type spinlock spinlock)
           (type event self))
  (assert (eql self (spinlock-owner spinlock)))
  (let* ((counter (atomic-incf (condvar-counter condvar) (ash 1 32)))
         (epoch   (ldb (byte 32 32) (+ counter (ash 1 32)))))
    (ensure-event (condvar-hint condvar) self)
    (release-spinlock spinlock)
    (loop named outer do
      (loop repeat 128 do
        (when (mod<= epoch (ldb (byte 32 0) (condvar-counter condvar)))
          (return-from outer))
        (spin-hint))
      (ensure-event-and-await (condvar-hint condvar) self 100))
    (maybe-clear-event (condvar-hint condvar) self)
    (get-spinlock spinlock self)))
