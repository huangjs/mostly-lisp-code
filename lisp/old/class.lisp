(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

;;; initialization
;;; &key must be used here.
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))

;; (defclass bank-account ()
;;   (customer-name
;;    balance))

(defclass checking-account (bank-account)
  ())

(defclass saving-account (bank-account)
  ())

(defparameter account (make-instance 'bank-account :customer-name "John"))

(setf (slot-value account 'customer-name) "John Doe")
(setf (slot-value account 'balance) 1000)

;;; with additional key parameter for initialization
(defmethod initialize-instance :after ((account bank-account)
                                       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
          (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(defparameter acct (make-instance
                    'bank-account
                    :customer-name "Sally Sue"
                    :balance 1000
                    :opening-bonus-percentage 5))

;;; accessor
(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

(defgeneric customer-name (account))

(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))

										;(defun (setf customer-name) (name account)
										;   (setf (slot-value account 'customer-name) name))

(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) name))


;;; with-slots with-accessors macros
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))

;; or using the two-item list form, like:
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots ((bal balance)) account
    (when (< bal *minimum-balance*)
      (decf bal (* bal .01)))))

;;; with-accessors
(defmethod merge-accounts ((account1 bank-account) (account2 bank-account))
  (with-accessors ((balance1 balance)) account1
	(with-accessors ((balance2 balance)) account2
	  (incf balance1 balance2)
	  (setf balance2 0))))


;;; inheritance
(defclass foo ()
  ((a :initarg :a :initform "A" :accessor a)
   (b :initarg :b :initform "B" :accessor b)))

;;; bar inherits from foo
(defclass bar (foo)
  ((a :initform (error "Must supply a value for a"))
   (b :initarg :the-b :accessor the-b :allocation :class)))


;;; don't use typep
(defmethod barp (object) nil)
(defmethod barp ((object bar)) t)

