;;; # Logged actions
;;;
;;; This STM uses a global Seqlock as the main concurrency control
;;; mechanism.  However, even when working with high-level atomic
;;; primitives, seqlocks have a problem with spurious conflucts, and
;;; not allowing *any* concurrent writes, even between fully independent
;;; transactions.
;;;
;;; As a way to patch this, an OO STM interface with rollback is also
;;; exposed.  You can pretty much do anything inside that interface,
;;; as long as it can be rolledback (unless it's in an irrevobable
;;; transaction), and never blocks indefinitely (unless it's in an
;;; irrevobable transaction).
;;;
;;; Rollbacks, commits and relinquishing locks are all logged with the
;;; same interface: implicit stacks of actions.
;;;
;;; An action is either: a function, a subtype of the structure type
;;; action, a cons or an arbitrary value.
;;;
;;; When an action is a function, it will be called with itself and
;;; a message to specify the operation to perform.  In addition to
;;; performing the operation (e.g. committing writes), it must return
;;; the next action in the queue.
;;;
;;; When it's a subtype of action, the function is action-fun will
;;; be called with action (the struct) and the operation. The return
;;; value is the next action.
;;;
;;; If it's a cons, the car will be called as an action, but the next
;;; action is the cdr of the cons (and the cdr is cleared).
;;;
;;; Finally, otherwise, the generic function action is called on the
;;; action with the operation as a second argument.
;;;
;;; The end of the stack is denoted with +last-action+, to simplify
;;; tracking whether an object is already in a stack of actions.
;;;
;;; There are currently two lists of actions:
;;; - %actions% holds a stack of commit/rollback-able actions; and
;;; - %locks% holds a stack of locks to release.
;;;
;;; Each stack is traversed exactly once per transaction, and cleared
;;; between each transaction.  Thus, actions that store the next action
;;; in a long-lived object can clear that data and avoid resource leaks.
;;;
;;; Actions in %actions% receive one of two messages: COMMIT after a
;;; successful transaction, and ROLLBACK on failure.
;;;
;;; Actions in %locks% only receive RELEASE as a message.  The explicit
;;; message is useful when the same action is both in %actions% and
;;; %locks%.
;;;
;;; Note that is a lock is only associated with a single object, it
;;; simplifies things quite a lot to release it as part of COMMIT or
;;; ROLLBACK, and not push anything on %locks%.

(in-package "STM")
(deftype action-function ()
  '(function (action t) (values t &optional)))
(defstruct (action
             (:constructor nil))
  (fun nil :type action-function))

(defconstant +last-action+ '+last-action+)

(defgeneric action (action message))

(declaim (inline send-action))
(defun send-action (action message)
  (declare (notinline send-action))
  (typecase action
    (cons      (prog1 (shiftf (cdr action) nil)
                 (send-action (car action) message)))
    (action    (funcall (action-fun action) action message))
    (function  (funcall action action message))
    (otherwise (action action message))))

(defun send-all-actions (actions message)
  (do ((action actions (send-action action message)))
      ((eq action +last-action+))))

(defun abort-transaction (&optional reason)
  "The next try will be irrevocable from the get go if REASON is T."
  (throw 'abort-transaction reason))
