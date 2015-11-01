;;; procedural oriented.
(defstruct account
  (name "") (balance 0.00) (interest-rate .06))

(defun account-withdraw (account amt)
  "Make a withdrawal from this account."
  (if (<= amt (account-balance account))
      (decf (account-balance account) amt)
      'insufficient-funds))

(defun account-deposit (account amt)
  "Make a deposit to this account."
  (incf (account-balance account) amt))

(defun account-interest (account)
  "Accumulate interest int this account."
  (incf (account-balance account)
        (* (account-interest-rate account)
           (account-balance account))))

;;; lisp style oo style
;;; message dispatch
(defun new-account (name &optional (balance 0.00)
                         (interest-rate .06))
  "Create a new account that knows the following message:"
  #'(lambda (message)
      (case message
        (withdraw #'(lambda (amt)
                      (if (<= amt balance)
                          (decf balance amt)
                          'insufficient-funds)))
        (deposit #'(lambda (amt) (incf balance amt)))
        (balance #'(lambda () balance))
        (name #'(lambda () name))
        (interest #'(lambda ()
                      (incf balance
                            (* interest-rate balance)))))))

(defun get-method (object message)
  "Return the method that implements message for this object."
  (funcall object message))

(defun send (object message &rest args)
  "Get the function to implement the message,
and apply the function to the args."
  (apply (get-method object message) args))

(deftest test-oo-closure ()
  (show
   (setf acct (new-account "J. Random Customer" 1000.00))
   (send acct 'withdraw 333.00)
   (send acct 'name)
   (send acct 'balance)
   ))

;;; the problem:
;;; messages are not functions hard to combine.

;;; the solution is to find the right function automatically.

;;; the function withdraw is GENERIC
(defun withdraw (object &rest args)
  "Define withdraw as a generic function on objects."
  (apply (get-method object 'withdraw) args))

;;; define a class macro
(defmacro define-class (class inst-vars class-vars &body methods)
  "Define a class for object-oriented programming."
  ;; Define constructor and generic function sfor methods
  `(let ,class-vars
    (mapcar #'ensure-generic-fn ',(mapcar #'first methods))
    (defun ,class ,inst-vars
      #'(lambda (message)
          (case message
            ,@(mapcar #'make-clause methods))))))

(defun make-clause (clause)
  "Translate a message from define-class into a case clause."
  `(,(first clause) #'(lambda ,(second clause) ,@(rest (rest clause)))))

(defun ensure-generic-fn (message)
  "Define an object-oriented dispatch function for a message,
unless it has already been defined as one."
  (unless (generic-fn-p message)
    (let ((fn #'(lambda (object &rest args)
                  (apply (get-method object message) args))))
      (setf (symbol-function message) fn)
      (setf (get message 'generic-fn) fn))))

(defun generic-fn-p (fn-name)
  "Is this a generic function?"
  (and (fboundp fn-name)
       (eq (get fn-name 'generic-fn) (symbol-function fn-name))))

(defvar *test-def-class*
  '(define-class account (name &optional (balance 0.00)) ((interest-rate .06))
    (withdraw (amt) (if (<= amt balance)
                        (decf balance amt)
                        'insufficient-funds))
    (deposit (amt) (incf balance amt))
    (balance () balance)
    (name () name)
    (interest () (incf balance (* interest-rate balance)))))

(deftest test-def-class ()
  (show
    (macroexpand-1 *test-def-class*)
    (setf acct2 (account "A. User" 2000.00))
    (deposit acct2 400.00)
    (interest acct2)
    (balance acct2)
    (balance acct)
    ))

;;; Delegation
;;; a password account
(define-class password-account (password account) ()
  (change-password (pass new-pass)
                   (if (equal pass password)
                       (setf password new-pass)
                       'wrong-password))
  ;; Note, otherwise may be a tenured symbol.
  ;; use otherwise in the case
  (otherwise (pass &rest args)
             (if (equal pass password)
                 ;; expand the macro and then you will see why message can be used here.
                 ;; it follows the message-sending protocal. 
                 (apply message account args)
                 'wrong-password)))

(deftest test-password-account ()
  (show
    (setf acct3 (password-account "secret" acct2))
    (balance acct3 "secret")
    (withdraw acct3 "guess" 2000.00)
    (withdraw acct3 "secret" 2000.00)
    ))

;;; limited account
(define-class limited-account (limit acct) ()
  (withdraw (amt)
            (if (> amt limit)
                'over-limit
                (withdraw acct amt)))
  (otherwise (&rest args)
             (apply message acct args)))

(deftest test-limited-account ()
  (show
    (setf acct4 (password-account "pass"
                                  (limited-account 100.00
                                                   (account "A. Thrifty Spender" 500.00))))
    (withdraw acct4 "pass" 200.00)
    (withdraw acct4 "pass" 20.00)
    (withdraw acct4 "guess" 20.00)
    ))

;;; problem
;;; no inheritance

;;; CLOS way
(defclass account2 ()
  ((name :initarg :name :reader name)
   (balance :initarg :balance :initform 0.00 :accessor balance)
   (interest-rate :allocation :class :initform .06 :reader interest-rate)))

(deftest test-clos ()
  (show
    (setf a1 (make-instance 'account2
                            :balance 5000.00
                            :name "Fred"))
    (name a1)
    (balance a1)
    (interest-rate a1)
    ))

(defmethod withdraw ((acct account2) amt)
  (if (< amt (balance acct))
      (decf (balance acct) amt)
      'insufficient-funds))

(defclass limited-account (account2)
  ((limit :initarg :limit :reader limit)))

(defmethod withdraw ((acct limited-account) amt)
  (if (> amt (limit acct))
      'over-limit
      (call-next-method)))

(deftest test-clos2 ()
  (show
    (setf a2 (make-instance 'limited-account
                            :name "A. Thrifty Spender"
                            :balance 500.00 :limit 100.00))
    (name a2)
    (withdraw a2 200.00)
    (withdraw a2 20.00)
    ))

;;; audited account
(defclass audited-account (account2)
  ((audit-trail :initform nil :accessor audit-trail)))

(defmethod withdraw :before ((acct audited-account) amt)
  (push (print `(withdrawing ,amt))
        (audit-trail acct)))

(defmethod withdraw :after ((acct audited-account) amt)
  (push (print `(withdraw (,amt) done))
        (audit-trail acct)))

(deftest test-clos3 ()
  (show
    (setf a3 (make-instance 'audited-account :balance 1000.00))
    (withdraw a3 100.00)
    (audit-trail a3)
    (setf (audit-trail a3) nil)
    ))

