(eql '#:foo '#:foo)                     ;--> NIL

common-lisp:*package*
cl:*package*

(mapcar #'package-name (package-use-list :cl-user))

(eql :a keyword:a)                      ;--> T

(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db))	;the use of :common-lisp package can be omitted or just type :use :cl.

(defpackage :com.gigamonkeys.text-db
  (:use :cl)
  (:export :open-db
           :save
           :store))

;;; import
(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db)
  (:import-from :com.acme.email :parse-email-address))

;;; shadow
(defpackage :com.gigamonkeys.email-db
  (:use
   :common-lisp
   :com.gigamonkeys.text-db
   :com.acme.text)
  (:import-from :com.acme.email :parse-email-address)
  (:shadow :build-index))	; this is a conflict that we want to avoid





