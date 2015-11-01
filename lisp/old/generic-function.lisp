(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))

;;; class topology:
;;;				bank-account
;;;             /         \
;;;            /           \
;;;           /             \
;;;   checking-account   saving-account
;;;
;;; basic function of the top class bank-account
(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

;;; subclass call super method.
(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

;;; delegation
(defmethod withdraw ((proxy proxy-account) amount)
  (withdraw (proxied-account proxy) amount))


;;; Specified type
(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)  ;changing the variable later won't change the method
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
    (call-next-method)))

;;; auxiliary method
;;; no need for call-next-method
(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

;;; example of simple method combination
(defgeneric priority (jobs)
  (:documentation "Return the priority at which the job should be run.")
  (:method-combination + :most-specific-last))

(defmethod priority + ((job express-job)) 10)

