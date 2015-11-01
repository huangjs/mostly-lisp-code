(require :aserve)

(defpackage :tutorial 
    (:use :common-lisp :excl :net.aserve :net.html.generator))

(in-package :tutorial)

(start :port 8000)

;;; hello world!
(publish :path "/hello"
		 :content-type "text/plain"
		 :function 
		 #'(lambda (req ent)
			 (with-http-response (req ent)
			   (with-http-body (req ent)
				 (princ "Hello World!" *html-stream*)))))


(publish :path "/hello2"
		 :content-type "text/html"
		 :function 
		 #'(lambda (req ent)
			 (with-http-response (req ent)
			   (with-http-body (req ent)
				 (html 
                  (:html (:head (:title "Hello World Test"))
                         (:body 
						  ((:font :color "red") "Hello ")
						  ((:font :color "blue") "World!"))))))))



(publish :path "/hello-count"
		 :content-type "text/html"
		 :function
		 (let ((count 0))
		   #'(lambda (req ent)
			   (with-http-response (req ent)
				 (with-http-body (req ent)
				   (html
					(:html
					 (:head (:title "Hello Counter"))
					 (:body 
					  ((:font :color (nth (random 5)
										  '("red" "blue" 
											"green" "purple" 
											"black")))
                       "Hello World had been called " 
                       (:princ (incf count)) 
                       " times")))))))))


(publish :path "/queryform"
		 :content-type "text/html"
		 :function
		 #'(lambda (req ent)
			 (let ((name (cdr (assoc "name" (request-query req) 
									 :test #'equal))))
			   (with-http-response (req ent)
				 (with-http-body (req ent)
				   (if* name
						then	  ; form was filled out, just say name
						(html (:html
							   (:head (:title "Hi to " (:princ-safe name)))
							   (:body "Your name is "
									  (:b (:princ-safe name))))) ;print-safe to print special char in html
						else			; put up the form
						(html (:html
							   (:head (:title "Tell me your name"))
							   (:body
								((:form :action "queryform"
										:method "GET")
								 "Your name is "
								 ((:input :type "text"
										  :name "name"
										  :maxlength "20"))))))))))))



(publish :path "/charcount"
		 :content-type "text/html"
		 :function
		 #'(lambda (req ent)
			 (let* ((body (get-request-body req))
					(text (if* body
							   then (cdr (assoc "quotation"
												(form-urlencoded-to-query body)
												:test #'equal)))))
			   (with-http-response (req ent)
				 (with-http-body (req ent)
				   (if* text
						then		   ; got the quotation, analyze it
						(html 
						 (:html
						  (:head (:title "Character Counts")
								 (:body 
								  (:table
								   (do ((i #.(char-code #\a) (1+ i)))
									   ((> i #.(char-code #\z)))
									 (html (:tr
											(:td (:princ (code-char i)))
											(:td (:princ 
												  (count (code-char i)
														 text)))))))))))
						else			; ask for quotation
						(html
						 (:html
						  (:head (:title "quote character counter")
								 (:body 
								  ((:form :action "charcount"
										  :method "POST"
										  ;; :enctype "multipart/form-data"
										  )
								   "Enter your favorite quote "
								   :br
								   ((:textarea
									 :name "quotation"
									 :rows 30
									 :cols 50))
								   :br
								   ((:input :type "submit"
											:name "submit"
											:value "count it")))))))))))))


(publish :path "/secret"
    :content-type "text/html"
    :function
    #'(lambda (req ent)
         (multiple-value-bind (name password) (get-basic-authorization req)
            (if* (and (equal name "foo") (equal password "bar"))
               then (with-http-response (req ent)
                      (with-http-body (req ent)
                        (html (:head (:title "Secret page"))
                              (:body "You made it to the secret page"))))
               else ; cause browser to ask name and password
                    (with-http-response (req ent :response 
                                                 *response-unauthorized*)
                      (set-basic-authorization req "secretserver")
                      (with-http-body (req ent)))))))


(publish :path "/local-secret"
		 :content-type "text/html"
		 :function
		 #'(lambda (req ent)
			 (let ((net-address (ash (socket:remote-host
									  (request-socket req))
									 -24)))
			   (if* (equal net-address 127)
					then (with-http-response (req ent)
						   (with-http-body (req ent)
							 (html (:head (:title "Secret page"))
								   (:body (:b "Congratulations. ")
										  "You are on the local network"))))
					else
					(with-http-response (req ent)
					  (with-http-body (req ent)
						(html
						 (:html (:head (:title "Unauthorized"))
								(:body 
								 "You cannot access this page "
								 "from your location")))))))))


;;; proxy
(net.aserve:start :port 8000 :keep-alive t
				  :proxy t
				  :cache '(:memory 10000000
						   :disk ("/tmp/mycache" 30000000)
						   :disk (nil 20000000)) ;name chosen by allegroserve
				  )


;;; setup virtual host explicitly
(defun setup-virtual-hosts (server)
  (let ((vhost-table (wserver-vhosts server))
		(foo-names '("localhost" "www.foo.com" "foo.com"))
		(bar-names '("www.bar.com" "store.bar.com")))
    (let ((default-vhost (wserver-default-vhost server)))
      (setf (vhost-names default-vhost) foo-names)
      (dolist (name foo-names)
		(setf (gethash name vhost-table) default-vhost)))
    (let ((bar-vhost (make-instance 'vhost :names bar-names)))
      (dolist (name bar-names)
		(setf (gethash name vhost-table) bar-vhost)))))

;;; charset/external-format
(publish :path "/queryform-utf8"
		 :content-type "text/html; charset=UTF-8" ; this is also necessary 
		 :function
		 #'(lambda (req ent)
			 (let ((name (cdr (assoc "name" (request-query req) 
									 :test #'equal))))
			   (with-http-response (req ent)
				 (with-http-body (req ent
									  ;; this is the charset
									  :external-format (crlf-base-ef :utf8))
				   (if* name
						then	  
						(html (:html
							   (:head (:title "Hi to " (:princ-safe name)))
							   (:body "Your name is "
									  (:b (:princ-safe name))))) 
						else			
						(html (:html
							   (:head (:title "Tell me your name"))
							   (:body
								((:form :action "queryform"
										:method "GET")n
								 "Your name is "
								 ((:input :type "text"
										  :name "name"
										  :maxlength "20"))))))))))))


