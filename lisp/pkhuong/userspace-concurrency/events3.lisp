#||
Events: robust, less race-prone, alternative to condition variables.

Events are not fair, and prone to spurious wake ups.  Fairness is
inherently fragile (a single thread can block the execution of arbitrarily
many, without even holding any resource except its rank in a queue);
spurious wake-ups happen.

Events are a combination of a counting semaphore and an epoch counter.

Waiting is typically structured as:
 grab the current epoch with EVENT-TOKEN
  ...perform work...
 wait on EVENT with token

EVENT-WAIT returns when any of the following is true:
 - the epoch is different from the one in the token
 - the semaphore has been successfully decremented
 - the operation times out

 ** The implementation is somewhat ABA-unsafe; the odds are very much
    on our side, but the time out is a good safety net.

 If it returns, it does so with a new token.

EVENT-SIGNAL signals a given number of waiters, while
EVENT-BROADCAST changes the epoch, signals all the waiters, and reset
the semaphore count.

Note that, unlike condition variables, the [...perform work...] section
is aware of signals as well, be it via the persistent semaphore counter,
or the epoch check.  Moreover, EVENT-TOKEN and EVENT-WAIT are free of
atomic operations, except when EVENT-WAIT returns by semaphore.  This
makes events well-suited to CAS-based protocols.

Implementation:
The current epoch, wake count and slow-path indicator are all packed in a
single fixnum.

The wake count makes up the top bits of the fixnum, so that it can be
increased with a single XADD.

The current epoch must be set with a CAS; in order to make sure that
doesn't block, an auxiliary "epoch" slot is present.  Changing the
epoch only has to modify that slot.  Once the epoch slot and the
current epoch in the packed fixnum mismatch, signallers and
broadcasters will both race to update the packed epoch.

Waiters have a separate woken counter.  It too is updated with a CAS
whenever a waiter attempts to use a signal.  That counter is on a
separate cache line to minimise conflicts between waiters and signallers
or broadcasters.

Events are lazily upgraded to point to a semaphore.  When they do point
to a semaphore, a single bit in the packed fixnum is used to indicate
whether there might be a thread waiting on the semaphore.
||#
(defpackage "SB-EVENT"
    (:use "CL" "SB-EXT" "SB-ALIEN")
  (:export "MAKE-EVENT" "EVENT" "EVENT-P"
           "EVENT-TOKEN"
           "EVENT-SIGNAL" "EVENT-BROADCAST"
           "EVENT-WAIT"))
(in-package "SB-EVENT")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (handler-bind ((simple-error (lambda (condition)
                                 (declare (ignore condition))
                                 (invoke-restart 'continue))))
    (sb-c:defknown word-compare-and-swap (system-area-pointer word word) word)
    (sb-c:defknown fx-xadd (system-area-pointer fixnum) fixnum))
  
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
      (sb-vm::move r eax)))

  (sb-c:define-vop (fx-xadd)
    (:translate fx-xadd)
    (:policy :fast-safe)
    (:args (sap :scs (sb-vm::sap-reg) :to :eval)
           (inc :scs (sb-vm::any-reg) :target r))
    (:arg-types system-area-pointer sb-vm::tagged-num)
    (:results (r :scs (sb-vm::any-reg) :from (:argument 1)))
    (:result-types sb-vm::tagged-num)
    (:generator 5
      (sb-vm::move r inc)
      (sb-assem:inst sb-vm::xadd (sb-vm::make-ea :qword :base sap) r :lock))))

(defun word-compare-and-swap (sap old new)
  (declare (type system-area-pointer sap)
           (word old new))
  (word-compare-and-swap sap old new))

(defun fx-xadd (sap inc)
  (declare (type system-area-pointer sap)
           (type fixnum inc))
  (fx-xadd sap inc))

(defmacro define-word-casser (name structure slot)
  (let ((addressor (intern (format nil "~A-~A-ADDRESSOR" structure slot))))
    `(progn
       (declaim (inline ,addressor ,name))
       (sb-kernel:define-structure-slot-addressor ,addressor :structure ,structure :slot ,slot)
       (defun ,name (,structure old new)
         (declare (type ,structure ,structure)
                  (type word old new)
                  (optimize speed))
         (sb-sys:with-pinned-objects (,structure)
           (word-compare-and-swap (sb-sys:int-sap (,addressor ,structure))
                                  old new))))))

(defmacro define-fixnum-xadder (name structure slot)
  (let ((addressor (intern (format nil "~A-~A-ADDRESSOR" structure slot))))
    `(progn
       (declaim (inline ,addressor ,name))
       (sb-kernel:define-structure-slot-addressor ,addressor :structure ,structure :slot ,slot)
       (defun ,name (,structure inc)
         (declare (type ,structure ,structure)
                  (type fixnum inc)
                  (optimize speed))
         (sb-sys:with-pinned-objects (,structure)
           (fx-xadd (sb-sys:int-sap (,addressor ,structure))
                    inc))))))

;;; C-side events (slow path)
;;;
(load-shared-object "/Users/pkhuong/userspace-concurrency/events.dylib")

(declaim (inline %event-init))
(define-alien-routine ("event_init" %event-init) int (event (* unsigned-long)))
(macrolet ((def-simple-event-routine (name)
             (let ((c-name (string-downcase
                              (substitute #\_ #\- (subseq (symbol-name name) 1)))))
               `(progn
                  (declaim (inline ,name))
                  (define-alien-routine (,c-name ,name) int (event unsigned-long)))))
           (def-routines (&rest names)
             `(progn
                ,@(mapcar (lambda (name)
                            `(def-simple-event-routine ,name))
                          names))))
  (def-routines %event-destroy %event-signal %event-broadcast %event-resignal %event-clear))
(declaim (inline %event-wait))
(define-alien-routine ("event_wait" %event-wait) int
  (event unsigned-long) (sec unsigned-long) (ns unsigned-long))

(defconstant +uninitialised-%event+ 0)

;;; Event
;;;
;;; A counting semaphore, and an epoch counter in one.
;;; Waiters wait until they either grab a counter from the
;;; semaphore, or the epoch changes.
;;; All values are only accessed with LOCK instructions,
;;; except when WOKEN is zeroed out, and that can be late:
;;; worst case, too many wake ups.
(defstruct (event
             (:constructor %make-event (%event))
             (:print-object print-event))
  (counts  0 :type fixnum)
  (epoch   0 :type fixnum)
  (%event  0 :type fixnum)
  (!!pad3  0 :read-only t)
  (!!pad4  0 :read-only t)
  (!!pad5  0 :read-only t)
  (!!pad6  0 :read-only t)
  (!!pad7  0 :read-only t)
  #-x86-64 (!!pad8  0 :read-only t)
  #-x86-64 (!!pad9  0 :read-only t)
  #-x86-64 (!!padA  0 :read-only t)
  #-x86-64 (!!padB  0 :read-only t)
  #-x86-64 (!!padC  0 :read-only t)
  #-x86-64 (!!padD  0 :read-only t)
  #-x86-64 (!!padE  0 :read-only t)
  #-x86-64 (!!padF  0 :read-only t)
  (woken   0 :type fixnum))
(define-fixnum-xadder event-counts-xadd event counts)

#+x86-64
(progn
  (defconstant +counts-wake-ups-width+ 30)
  (defconstant +counts-wake-ups-shift+ 31)

  (defconstant +counts-epoch-width+ 30)
  (defconstant +counts-epoch-shift+ 1)

  (defconstant +counts-slow-width+ 1)
  (defconstant +counts-slow-shift+ 0)

  (defconstant +max-waiters+ (ash 1 28)))

#-x86-64
(progn
  (defconstant +counts-wake-ups-width+ 15)
  (defconstant +counts-wake-ups-shift+ 15)

  (defconstant +counts-epoch-width+ 14)
  (defconstant +counts-epoch-shift+ 1)

  (defconstant +counts-slow-width+ 1)
  (defconstant +counts-slow-shift+ 0)

  (defconstant +max-waiters+ (ash 1 13)))

(defmacro counts-wake-ups (x)
  `(ldb (byte +counts-wake-ups-width+ +counts-wake-ups-shift+) ,x))
(defmacro counts-epoch (x)
  `(ldb (byte +counts-epoch-width+ +counts-epoch-shift+) ,x))
(defmacro counts-slow (x)
  `(ldb (byte +counts-slow-width+ +counts-slow-shift+) ,x))
(defmacro epoch-epoch (x)
  `(ldb (byte +counts-epoch-width+ 0) ,x))
(defmacro woken-woken (x)
  `(ldb (byte +counts-wake-ups-shift+ 0) ,x))

(declaim (inline mod< mod-min))
(defun mod< (x y width)
  (>= (ldb (byte width 0) (- x y))
      (ash 1 (1- width))))
(defun mod-min (x y width)
  (if (mod< x y width) x y))

(defun print-event (event stream)
  (print-unreadable-object (event stream :type t :identity t)
    (let ((counts (event-counts event)))
      (format stream "[~A] ~A:~A ~A:~A - ~A"
              (if (plusp (counts-slow counts)) "SLOW" "FAST")
              
              (counts-epoch counts)
              (epoch-epoch (event-epoch event))
              
              (counts-wake-ups counts)
              (woken-woken (event-woken event))

              (event-%event event)))))

(defun make-event ()
  (%make-event +uninitialised-%event+))

(defun %upgrade-event (event)
  (declare (type event event))
  (when (= +uninitialised-%event+ (event-%event event))
    (sb-sys:without-interrupts
      (with-alien ((%event unsigned-long))
        (assert (zerop (%event-init (addr %event))))
        (assert (not (eql +uninitialised-%event+ %event)))
        (assert (<= %event most-positive-fixnum))
        (let ((%event %event))
          (cond ((= +uninitialised-%event+
                    (compare-and-swap (event-%event event)
                                      +uninitialised-%event+ %event))
                 (finalize event (lambda ()
                                   (%event-destroy %event))))
                (t
                 (%event-destroy %event))))))))

(declaim (inline ensure-%event))
(defun ensure-%event (event)
  (declare (type event event))
  (let ((%event (event-%event event)))
    (cond ((eql %event +uninitialised-%event+)
           (%upgrade-event event)
           (event-%event event))
          (t
           %event))))

(defvar *max-concurrent-signals* 4)

(defun %signal-upgraded-event (event n)
  (let ((%event (event-%event event))
        (max    *max-concurrent-signals*))
    (assert (/= +uninitialised-%event+ %event))
    (dotimes (i (if n
                    (min n max)
                    max))
      (%event-signal %event))))

(declaim (inline signal-%event))
(defun signal-%event (event counts n)
  (declare (type event event)
           (type fixnum counts))
  (when (oddp counts)
    (%signal-upgraded-event event n)))

(defun %maybe-advance-epoch (event)
  (declare (type event event))
  (let* ((cur-epoch  (event-epoch event))
         (counts     (event-counts event))
         (pub-epoch  (counts-epoch counts))
         (new-counts (ash cur-epoch +counts-epoch-shift+)))
    (unless (mod< pub-epoch cur-epoch +counts-epoch-width+)
      (return-from %maybe-advance-epoch nil))
    (loop
      (let* ((counts     (event-counts event))
             (pub-epoch  (counts-epoch counts)))
        (unless (mod< pub-epoch cur-epoch +counts-epoch-width+)
          (return t))
        (when (= counts (compare-and-swap (event-counts event)
                                          counts
                                          new-counts))
          (signal-%event event counts nil)
          (setf (event-woken event) 0)
          (return t))))))

(declaim (inline maybe-advance-epoch))
(defun maybe-advance-epoch (event)
  (declare (type event event))
  (let* ((cur-epoch  (event-epoch event))
         (counts     (event-counts event))
         (pub-epoch  (counts-epoch counts)))
    (and (mod< pub-epoch cur-epoch +counts-epoch-width+)
         (%maybe-advance-epoch event))))

(defun event-token (event)
  (declare (type event event))
  (counts-epoch (event-counts event)))

(defun event-signal (event n)
  (declare (type event event)
           (type (integer 0 #. +max-waiters+) n))
  (unless (maybe-advance-epoch event)
    (signal-%event event
                   (event-counts-xadd event
                                      (ash n +counts-wake-ups-shift+))
                   n)))

(defun event-broadcast (event prev-epoch)
  (declare (type event event)
           (type (or null (unsigned-byte #. +counts-epoch-width+))
                 prev-epoch))
  (unless prev-epoch
    (setf prev-epoch (counts-epoch (event-counts event))))
  (when (= prev-epoch (event-epoch event))
    (compare-and-swap (event-epoch event)
                      prev-epoch
                      (ldb (byte +counts-epoch-width+ 0) (1+ prev-epoch))))
  (maybe-advance-epoch event))

;;; EVENT-WAIT
(declaim (ftype (function (event (unsigned-byte #. +counts-epoch-width+) t)
                          (values t fixnum &optional))
                event-wait-fast-path-1))
(defun event-wait-fast-path-1 (event prev-epoch resignal)
  (declare (type event event)
           (type (unsigned-byte #. +counts-epoch-width+) prev-epoch)
           (optimize speed))
  (flet ((resignal ()
           (let ((%event (event-%event event)))
             (%event-signal %event))))
    (declare (inline resignal))
    (let* ((counts (event-counts event))
           (epoch  (counts-epoch counts))
           (wakes  (counts-wake-ups counts)))
      (cond ((/= epoch prev-epoch)
             (when resignal
               (resignal))
             (values t counts))
            (t
             (let ((woken (event-woken event)))
               (cond ((and (mod< woken wakes +counts-wake-ups-width+)
                           (= woken (compare-and-swap (event-woken event)
                                                      woken
                                                      (ldb (byte +counts-wake-ups-width+ 0)
                                                           (1+ woken)))))
                      (when resignal
                        (resignal))
                      (values t counts))
                     (t (values nil counts)))))))))

(defun busy-loop (event prev-epoch iter)
  (declare (type event event)
           (type (unsigned-byte #. +counts-epoch-width+)
                 prev-epoch)
           (type (and unsigned-byte fixnum) iter)
           (values (or null fixnum) &optional)
           (optimize speed))
  (let ((counts 0))
    (declare (type fixnum counts)
             (optimize (safety 0)))
    (loop named outer
          do
       (multiple-value-bind (donep cur-counts)
           (event-wait-fast-path-1 event prev-epoch nil)
         (when donep (return-from outer (counts-epoch cur-counts)))
         (setf counts cur-counts))
       (loop
         (cond ((/= counts (event-counts event))
                (return))
               ((= iter 0)
                (return-from outer))
               (t
                (decf iter)
                (spin-loop-hint)))))))

(defun sleepy-loop (event prev-epoch)
  (declare (type event event)
           (type (unsigned-byte #. +counts-epoch-width+)
                 prev-epoch)
           (values (or null fixnum) &optional)
           (optimize speed))
  (let ((%event (event-%event event))
        (counts 0)
        (resignal nil))
    (declare (type fixnum counts)
             (optimize (safety 0)))
    (loop
      (multiple-value-bind (donep cur-counts)
          (event-wait-fast-path-1 event prev-epoch nil)
        (when donep
          (when resignal
            (%event-signal %event))
          (return (counts-epoch cur-counts)))
        (setf counts cur-counts))
      (loop
        (when (/= counts (event-counts event))
          (return))
        (setf resignal (zerop (%event-wait %event 1 0)))))))

(declaim (maybe-inline try-upgrade-event))
(defun try-upgrade-event (event prev-epoch)
  (declare (type event event)
           (type (unsigned-byte #. +counts-epoch-width+)
                 prev-epoch)
           (optimize speed))
  (let* ((counts (event-counts event))
         (epoch  (counts-epoch counts)))
    (and (= epoch prev-epoch)
         (or (oddp counts)
             (compare-and-swap (event-counts event)
                               counts
                               (logior counts 1))))))

(defun event-wait (event prev-epoch)
  (declare (type event event)
           (type (or null (unsigned-byte #. +counts-epoch-width+))
                 prev-epoch)
           (optimize speed (space 0))
           (values (or null fixnum) &optional))
  (let ((prev-epoch (ash (or prev-epoch
                             (counts-epoch (event-counts event)))
                         1)))
    (flet ((poll ()
             (multiple-value-bind (donep counts)
                 (event-wait-fast-path-1 event prev-epoch nil)
               (when donep
                 (return-from event-wait (counts-epoch counts))))))
      (declare (inline poll))
      (let ((counts (busy-loop event prev-epoch 1024)))
        (when counts
          (return-from event-wait counts)))
      (sb-thread:thread-yield)
      (poll)
      (ensure-%event event)
      (poll)
      (loop
        (when (try-upgrade-event event prev-epoch)
          (return))
        (poll)
        (spin-loop-hint))
      (sleepy-loop event prev-epoch))))
