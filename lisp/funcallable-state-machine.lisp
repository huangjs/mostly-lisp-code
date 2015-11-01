(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :closer-mop))

(defclass basic-state-machine (c2mop:funcallable-standard-object)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defclass standard-state-machine (basic-state-machine)
  ((state :initform :initial
          :initarg :state
          :accessor state-of)
   (last-event :initform (get-internal-real-time)
               :accessor last-event-of))
  (:metaclass c2mop:funcallable-standard-class))

(defgeneric standard-state-machine-event (machine state event))

(defmethod initialize-instance :before ((machine standard-state-machine) &key)
  (c2mop:set-funcallable-instance-function
   machine
   (lambda (event)
     (multiple-value-bind (next-state recur-p)
         (standard-state-machine-event machine (state-of machine) event)
       ;;
       (setf (last-event-of machine) (get-internal-real-time)
             (state-of machine) (or next-state (state-of machine)))
       ;;
       (if recur-p
           (funcall machine event)
           (values machine (state-of machine)))))))

(defmacro defstate (machine-type state-name (machine-sym event-sym) &body body)
  `(defmethod standard-state-machine-event ((,machine-sym ,machine-type) (state (eql ,state-name)) ,event-sym)
     ,@body))

(defstate standard-state-machine :initial (fsm event)
  nil)

(defmacro deffsm (name parents slots &rest options)
  `(defclass ,name ,(append (list 'standard-state-machine) parents)
     ,slots
     (:metaclass c2mop:funcallable-standard-class)
     ,@options))

(deffsm hi-fsm ()
  ())

(defstate hi-fsm :initial (fsm c)
  (case c
    (#\! :error)
    (#\H :want-i)))

(defstate hi-fsm :want-i (fsm c)
  (if (char-equal c #\i)
      :done
      (values :initial t)))

(defstate hi-fsm :done (fsm c)
  (let ((fsm (make-instance 'hi-fsm))
        (input "Oh? Hello there. Hi. How are you!?"))
    (map 'list 
         (lambda (c) 
           (if (eql :error (state-of fsm))
               (format t "Skipping: ~S~%" c)
               (funcall fsm c))) 
         input)
    (state-of fsm)))

