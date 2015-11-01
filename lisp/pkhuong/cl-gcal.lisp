;;; License: Modified BSD
;;; License Copyright (c) 2010, Paul-Virak Khuong
;;;  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; Neither the name of the Million Monkey Enterprises nor the names of
;;; its contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This is a mostly-working script; it's not packaged nor modularised.
;;; Feel free to use it however you want.

;;; deps:
;;; drakma
;;; cxml, cxml-stp
;;; cl-json
;;; cl-ppcre
;;; mel-base
;;; cl-base64

;;; TODO: Quote 5@7, etc.
;;; Special case to still keep time info?
;;; Replace 17h00 -> 17:00 in title, date
;;; Encoding!

(defclass gstate ()
  ((cookie-jar :reader   jar-of   :initform (make-instance 'drakma:cookie-jar))
   (magic      :accessor magic-of :initform nil)))

(defun login (email pw name
              &optional (state (make-instance 'gstate)))
  (multiple-value-bind (body code)
      (drakma:http-request
       "https://www.google.com/accounts/ClientLogin"
       :method :post
       :external-format-out :utf-8
       :parameters `(("Email"   . ,email)
                     ("Passwd"  . ,pw)
                     ("source"  . ,name)
                     ("accountType" . "HOSTED_OR_GOOGLE")
                     ("service" . "cl"))
       :cookie-jar (jar-of state))
    (assert (= 200 code))
    (cl-ppcre:do-register-groups (magic)
        ("Auth=(.*)" body (error "No Auth magic line!?"))
      (setf (magic-of state) magic)
      (return state))))

(defvar *gstate*)

(defmacro with-login ((email pw name) &body body)
  `(let ((*gstate* (login ,email ,pw ,name)))
     ,@body))

(defun gcal-request (uri &key
                     (method :get)
                     expected-code
                     (sink (cxml-stp:make-builder))
                     content
                     parameters)
  (assert (not (and content parameters)))
  (multiple-value-bind (body code headers)
      (apply #'drakma:http-request
             uri
             :method method
             :force-binary t
             :external-format-out :utf-8
             :external-format-in  :utf-8
             :redirect-methods '(:get :head :put :post)
             :cookie-jar (jar-of *gstate*)
             :additional-headers `(("Authorization"
                                    . ,(format nil "GoogleLogin auth=~A"
                                               (magic-of *gstate*)))
                                   ("GData-Version" . "2"))
             (cond (content
                    `(:content ,(if (typep content 'cxml-stp:node)
                                    (cxml-stp:serialize
                                     content
                                     (cxml:make-octet-vector-sink))
                                    content)
                      :content-type "application/atom+xml"))
                   (parameters
                    `(:parameters ,parameters))
                   (t '())))
    (when expected-code
      (assert (= code expected-code)))
    (values (if sink
                (cxml:parse body sink)
                body) 
            headers
            code)))

(defparameter *default-quickadd*
  (cxml:parse
   "<entry xmlns='http://www.w3.org/2005/Atom' xmlns:gCal='http://schemas.google.com/gCal/2005'>
  <content type=\"html\"></content>
  <gCal:quickadd value=\"true\"/>
</entry>
"
   (cxml-stp:make-builder)))

(defun make-quickadd-stp (text)
  (let* ((new-quickadd (cxml-stp:copy *default-quickadd*))
         (content      (cxml-stp:find-recursively
                        "content" new-quickadd
                        :key (lambda (x)
                               (and (typep x 'cxml-stp:element)
                                    (cxml-stp:local-name x)))
                        :test #'equal)))
    (cxml-stp:append-child content
                           (cxml-stp:make-text text))
    new-quickadd))

(defun quickadd (text)
  (multiple-value-bind (result headers)
      (gcal-request "http://www.google.com/calendar/feeds/default/private/full"
                    :method :post
                    :content (make-quickadd-stp text)
                    :expected-code 201)
    (values result (cdr (assoc :etag headers)))))

(defun event-update-uri (event)
  (let ((link (cxml-stp:find-recursively-if
               (lambda (node)
                 (and (typep node 'cxml-stp:element)
                      (equal (cxml-stp:attribute-value node "rel")
                             "edit")))
               event)))
    (cxml-stp:attribute-value link "href")))

(defun update-event (event &key title content where)
  (setf event (cxml-stp:copy event))
  (flet ((update (name value &optional #+nil type)
           (let ((parent
                  (cxml-stp:find-recursively-if
                   (lambda (node)
                     (and (typep node 'cxml-stp:element)
                          (equal (cxml-stp:local-name node) name)))
                   event)))
             (map nil #'cxml-stp:detach (cxml-stp:list-children parent))
             (cxml-stp:append-child parent
                                    (cxml-stp:make-text value))
             #+nil(when type
                    (setf (cxml-stp:attribute-value parent "type") type))
             event)))
    (when title
      (update "title" title))
    (when content
      (update "content" content))
    (when where
      (let ((where-node (cxml-stp:find-recursively-if
                         (lambda (node)
                           (and (typep node 'cxml-stp:element)
                                (equal (cxml-stp:local-name node) "where")))
                         event)))
        (setf (cxml-stp:attribute-value where-node "valueString") where)))
    event))

(defun gcal-update-event (event &rest keys &key title content where)
  (declare (ignore title content where))
  (gcal-request (event-update-uri event)
                :method :put
                :content (apply #'update-event event keys)
                :expected-code 200))

(defun gtranslate (string &key (from "fr") (to "en"))
  (multiple-value-bind (body code)
      (drakma:http-request "http://ajax.googleapis.com/ajax/services/language/translate"
                           :external-format-in  :utf-8
                           :external-format-out :utf-8
                           :parameters
                           `(("v"        . "1.0")
                             ("q"        . ,string)
                             ("langpair" . ,(format nil "~A|~A" from to)))
                           :additional-headers
                           '(("Referer"  . "http://www.faecum.qc.ca")))
    (assert (= 200 code))
    (cdr (assoc :translated-text
                (cdr (assoc :response-data
                            (json:decode-json-from-string body)))))))

(defun parse-body-for-location (body)
  (cl-ppcre:register-groups-bind (nil location)
      ("(?i)(?m)^(où|ou|endroit|lieu)\\s*:*\\s*(.*)$" body)
    location))

(defun parse-body-for-time (body)
  (cl-ppcre:register-groups-bind (nil time)
      ("(?i)(?m)^(quand|date|horaire|heure)\\s*:*\\s*(.*)$" body)
    time))

;; google doesn't like *escaped* <> in text
(defun filter-<> (string)
  (cl-ppcre:regex-replace-all "(\\w)>"
                              (cl-ppcre:regex-replace-all "<(\\w)"
                                                          string
                                                          "< \\1")
                              "\\1 >"))

(defun filter-magic-words (string)
  (macrolet ((filter (regex args
                            &body body)
               `(setf string
                      (cl-ppcre:regex-replace-all
                       ,regex string
                       ,(if (stringp args)
                            args
                            `(lambda ,args
                               ,@body))
                       :simple-calls t))))
    (filter "\"?(\\d+)\\s*@\\s*(\\d+)\"?"
            (match h1 h2)
      (declare (ignore h1 h2))
      (unless (eql #\" (aref match 0))
        (setf match (concatenate 'string "\"" match)))
      (unless (eql #\" (aref match (1- (length match))))
        (setf match (concatenate 'string match "\"")))
      match)
    (filter "(\\d+)h(\\d*)" (s h m)
            (declare (ignore s))
            (format nil "~A:~A"
                    h (if (= (length m) 0) "00" m)))
    (filter "(?i)\\d+(:\\d\\d)?\\s+à\\s+\\d+(:\\d\\d)?"
            (match _ __)
            (declare (ignore _ __))
            (format nil "depuis ~A" match))))

(defun add-simple-event (title body)
  (let* ((location      (parse-body-for-location body))
         (time          (parse-body-for-time body))
         (english-time  (and time
                             (gtranslate (filter-magic-words time))))
         (english-title (gtranslate (filter-magic-words title)))
         (complete-title (if time
                             (format nil "~A, on ~A"
                                     english-title english-time)
                             english-title))
         (simple-event  (quickadd complete-title)))
    (when location
      (setf location (format nil "~A loc: Montreal" location)))
    (gcal-update-event simple-event
                       :title (filter-<> title)
                       :content (filter-<> body)
                       :where location)
    complete-title))

(defun test (email pw)
  (with-login (email pw "faecum-cvc_email-0")
    (add-simple-event
     "5@7 IRO ce vendredi dÃ¨s 5h"
     "
Où: Pavillon André-Aisenstadt
Quand: Vendredi, 5PM-11PM
")))

(defmacro with-pop3-folder
    ((var (username password &key (host "localhost") (port 1110)))
     &body body)
  (let ((folder (gensym "FOLDER")))
    `(let ((,folder (mel:make-pop3-folder :username ,username
                                          :password ,password
                                          :host ,host :port ,port)))
       (unwind-protect
            (let ((,var ,folder))
              ,@body)
         (mel:close-folder ,folder)))))

(defun slurp-stream (stream &key (size-limit 4096))
  (let* ((string (make-array size-limit :element-type 'character))
         (actual-size (read-sequence string stream)))
    (sb-kernel:%shrink-vector string actual-size)))

(defun read-message (message)
  (values (slurp-stream (mel:message-header-stream message))
          (slurp-stream (mel:message-body-stream message))))

(defun quoted-printable-string-to-string (body)
  (let ((string (make-array (length body) :element-type 'character))
        (out    0))
    (do ((i 0 (1+ i)))
        ((>= i (length body))
           (sb-kernel:%shrink-vector string out))
      (let* ((char (aref body i))
             (decoded-char
              (if (char= char #\=)
                  (cond ((>= (1+ i) (length body)) char)
                        ((member (aref body (1+ i))
                                 '(#\Return #\Newline))
                         (incf i 2)
                         nil)
                        (t
                         (let ((num (parse-integer body
                                                   :start (1+ i)
                                                   :end (+ i 3)
                                                   :radix 16
                                                   :junk-allowed t)))
                           (incf i 2)
                           (code-char num))))
                  char)))
        (when decoded-char
          (setf (aref string out) decoded-char)
          (incf out))))))

(defun decode-body (body encoding)
  (declare (type string body)
           (type (or string null) encoding))
  (cond ((string= encoding "quoted-printable")
         (quoted-printable-string-to-string body))
        ((string= encoding "base64")
         (cl-base64:base64-string-to-string body))
        (t body)))

(declaim (inline prefixp suffixp))
(defun prefixp (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))
(defun suffixp (suffix string)
  (and (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string)
                                         (length suffix)))))

(defun decode-header (header)
  (macrolet ((wrap-match ((var prefix suffix) &body body)
               `(when (and ,(if prefix
                                `(prefixp ,prefix header)
                                t)
                           ,(if suffix
                                `(suffixp ,suffix header)
                                t))
                  (let ((,var (subseq header
                                      (length ,prefix)
                                      (- (length header)
                                         (length ,suffix)))))
                    ,@body))))
    (cl-ppcre:regex-replace-all
     "(?i)=\\?ISO-8859-1\\?(.)\\?(.*?)\\?="
     header
     (lambda (match type body)
       (declare (ignore match))
       (case (aref type 0)
         ((#\b #\B)
            (cl-base64:base64-string-to-string body))
         ((#\q #\Q)
            (nsubstitute
             #\Space #\_
             (quoted-printable-string-to-string body)))
         (otherwise body)))
     :simple-calls t)))

(defun recode-iso-8859-1 (string &key (external-format :default))
  (declare (type string string))
  (when (member external-format '(:iso-8859-1 nil))
    (return-from recode-iso-8859-1 string))
  (sb-ext:octets-to-string (map '(simple-array (unsigned-byte 8) 1)
                                #'char-code string)
                           :external-format external-format))

(defun fix-newlines (string)
  (nsubstitute #\newline #\return
               (cl-ppcre:regex-replace-all
                #.(coerce #(#\Return #\Newline) 'simple-string)
                string
                #.(coerce #(#\Newline) 'simple-string))))

(defmacro parse-header ((header-var name) (value) &body body)
  `(let ((foundp nil)
         ,value)
     (setf
      ,value
      (with-output-to-string (.out.)
        (with-input-from-string (.s. ,header-var)
          (loop for line = (read-line .s. nil nil)
                while line
                do (if foundp
                       (cl-ppcre:register-groups-bind
                           (nil .hit.)
                           ("(?m)(^\\s+(.*)$)?" line)
                         (if .hit.
                             (format .out. "~A" .hit.)
                             (return)))
                       (cl-ppcre:register-groups-bind
                           (.hit.)
                           (,(format nil "(?m)^~A:\\s*(.*)$"
                                     name)
                             line)
                         (setf foundp t)
                         (format .out. "~A" .hit.)))))))
     (unless foundp (setf ,value nil))
     (locally ,@body)))

(defun parse-message (message)
  (multiple-value-bind (header body)
      (read-message message)
    (macrolet
        ((look-for-header ((&rest headers) &body body)
                                        ;(var name &optional default)
           `(let ,(mapcar (lambda (spec)
                            `(,(first spec) ,(third spec)))
                   headers)
              ,@(mapcar
                 (lambda (spec)
                   (destructuring-bind (var name) spec
                     `(parse-header (header ,name) (.hit.)
                        (setf ,var .hit.))))
                 headers)
              (locally ,@body))))
      (look-for-header ((from "From")
                        (subject "Subject")
                        (encoding "Content-Transfer-Encoding")
                        (content-type "Content-Type"))
        (let (charset)
          (cl-ppcre:register-groups-bind (hit nil)
              ("charset=(.*?)($|;|\\s)" content-type)
            (setf charset hit))
          (values
            (decode-header subject) (decode-header from)
            (fix-newlines
             (recode-iso-8859-1
              (decode-body body encoding)
              :external-format
              (cond ((string-equal charset "UTF-8")
                     :utf-8)
                    (t
                     :iso-8859-1))))
            header))))))

(defun process-message-for-gcal (message)
  (multiple-value-bind (subject from body header)
      (parse-message message)
    (format t "subject: ~A~%from: ~A~%~A~%~%~A~%~%" subject from header body)
    (let ((complete-title (add-simple-event subject
                                            (format nil "~A~%~%De: ~A~%" body from))))
      (format nil "Title:~A~%Sujet: ~A~%De: ~A~%~%~A~%"
              complete-title subject from body))))

(defun process-pop3-mailbox (username password &key (host "localhost") (port 1110))
  (with-login (username password "calendar_app-faecum-1")
    (with-pop3-folder (folder (username password :host host :port port))
      (let ((outputs '()))
        (mel:map-messages (lambda (message)
                            (push (process-message-for-gcal message) outputs))
                          folder)
        (nreverse outputs)))))
