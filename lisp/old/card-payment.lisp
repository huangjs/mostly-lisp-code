(cl:defpackage :cl-charge 
  (:use :common-lisp 
        :trivial-https 
        :it.bese.arnesi 
        :cl-ppcre) 
  (:import-from :split-sequence 
                :split-sequence)) 

(cl:in-package :cl-charge) 

(defclass merchant () ()) 

(defclass http-merchant-mixin () 
  ((url :accessor url :initform 
		"https://www3.moneris.com/HPPDP/index.php"))) 

(defclass payment-method () ()) 

(defclass credit-card (payment-method) 
  ((card-number :accessor card-number :initarg :card-number) 
   (expiry-month :accessor expiry-month :initarg :expiry-month) 
   (expiry-year :accessor expiry-year :initarg :expiry-year))) 

(defparameter *test-mastercard* 
  (make-instance 'credit-card 
                 :card-number 4242424242424242 
                 :expiry-month 12 
                 :expiry-year 10)) 

(defclass transaction () 
  ((items :accessor items :initarg :items :initform nil 
          :documentation 
          "list of transaction items "))) 

(defclass transaction-item () 
  ((total-charge :accessor total-charge :initform 0 :initarg 
				 :total-charge))) 

(defmethod total-charge ((transaction transaction)) 
  (reduce #'+ (mapcar #'total-charge (items transaction)))) 

(defparameter *test-transaction* 
  (make-instance 
   'transaction 
   :items (list (make-instance 'transaction-item 
                               :total-charge 50.00)))) 

(defgeneric make-request (merchant payment-method transaction)) 

;;;; * Moneris Implementation 
;;;; Based on the Moneris DirectPost implementation guide 
;;;; Section 7 Sending a Transaction to the DirectPost solution 

(defclass moneris (merchant http-merchant-mixin) 
  ((store-id :accessor store-id :initform "xxxxxxx") 
   (key :accessor key :initform "xxxxxx"))) 

(defparameter *moneris* (make-instance 'moneris)) 

(defun list->http-get-parameters (list &key (prefix "?")) 
  (when list 
    (with-output-to-string (s) 
      (when prefix 
        (write-string prefix s) 
        (loop for (key val &rest  rest) on list 
		   by #'cddr 
		   if val 
		   do (format s "~A=~A~:[~;&~]" 
					  (escape-as-uri (strcat key)) 
					  (escape-as-uri (strcat val)) 
					  rest)))))) 

(defmethod make-http-get-url (merchant &rest parameters) 
  (with-output-to-string (s) 
    (write-string (url merchant) s) 
    (write-string (list->http-get-parameters 
				   `("ps_store_id" ,(store-id merchant) 
								   "hpp_key" ,(key merchant))) s) 
    (when parameters 
      (write-string (list->http-get-parameters parameters :prefix "&") 
					s)))) 

(defun parse-moneris-response (response-text) 
  (mapcar #'(lambda (line) 
              (setf line (mapcar #'trim-string (split-sequence #\= line))) 
              (cons (intern (string-upcase (first line)) 
                            (find-package :cl-charge)) (second line))) 
          (split "<br>" response-text))) 

(defmethod make-request ((merchant moneris) (card credit-card) 
						 transaction) 
  (apply #'make-http-get-url merchant `("charge_total" ,(format nil 
																"~$" (total-charge transaction)) 
													   "cc_num" ,(card-number card) 
													   "expMonth" ,(expiry-month card) 
													   "expYear" ,(expiry-year card)))) 
(defmethod perform-request (request) 
  (let* ((response (trivial-https:HTTP-GET request)) 
         (http-code (car response)) 
         (stream (caddr 
				  response)) 
         (response-text 
          (with-output-to-string (s) 
            (loop for line = (read-line stream nil) 
			   while line do (write-string line s))))) 
    (when (eql 200 http-code) 
	  (parse-moneris-response response-text)))) 

(defmethod charge-transaction ((merchant moneris) payment-method 
							   transaction) 
  (let* ((results (perform-request (make-request merchant 
												 payment-method transaction))) 
         (response-code (cdr (assoc 'response_code results :test #'equal))) 
         (response-number (and response-code 
                               (parse-integer response-code :junk-allowed t)))) 

    (if (and response-code response-number 
             (> 50 response-number)) 
        (values t results) 
        (values nil results)))) 

(defvar *fields* 
  '(("charge_total" :required t) 
    ("cc_num" :required t) 
    ("expMonth" :required t) 
    ("expYear" :requried t))) 
