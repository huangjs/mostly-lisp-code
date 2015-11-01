(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (REQUIRE :ACACHE "acache-2.1.5.fasl")
    ;;(require :acache)
    (use-package :db.allegrocache)
    (use-package :db.allegrocache.utils)
    (setf (sys:gsgc-switch :print) t)
    (setf (sys:gsgc-switch :verbose) t)
    ;;(setf (sys:gsgc-switch :auto-step) t)
    ))

;;; 次のような class を考えます。
(defclass ping-node ()
  ((ipaddr :initarg :ipaddr :accessor ping-ipaddr :index :any)
   (ut :initarg :ut :accessor ping-ut :index :any)
   (delay :initarg :delay :accessor ping-delay)
   (reply-p :initarg :reply-p :accessor ping-reply-p))
  (:metaclass persistent-class))

;;; まず open をして、
(defun initialize ()
  (open-file-database "/tmp/testit" :if-does-not-exist :create :if-exists :supersede :object-cache-size 110000 :class-cache-size 5000000))

;;; make-instance を繰替えす
(defun foo ()
  (loop repeat 60			; (* 24 60)
     do (time
	 (progn
	   (commit :bulk-load :start)
	   (flush-object-cache :all t)
	   (loop for i from 0 below 100000
	      do (make-instance 'ping-node
				:ipaddr i
				:ut (get-universal-time)
				:delay (random 200)
				:reply-p (random 2)))
	   (commit :sync nil)
	   (commit :bulk-load :end)
	   (gc t)
	   ))))

(defun clean-up ()
  (close-database)
  (gc t))

;;; type declaration in slots
