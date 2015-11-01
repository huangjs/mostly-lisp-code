;;; a patch to allegroserve's http client

;;; first make-http-client-request
;;; then do-http-request-keep-alive
;;; ...
;;; finally close-socket

(in-package :net.aserve.client)




(defun make-http-client-request (uri &key 
				     (method  :get)  ; :get, :post, ....
				     (protocol  :http/1.1)
				     keep-alive 
				     (accept "*/*") 
				     cookies  ; nil or a cookie-jar
				     basic-authorization
				     digest-authorization
				     content
				     content-length 
				     content-type
				     query
				     headers
				     proxy
				     proxy-basic-authorization
				     user-agent
				     (external-format 
				      *default-aserve-external-format*)
					 socket				;this is new
				     ssl
				     timeout
				     )
  

  (declare (ignorable timeout))
  
  (let (host (sock socket) port fresh-uri scheme-default-port)
    ;; start a request 
  
    ; parse the uri we're accessing
    (if* (not (typep uri 'net.uri:uri))
       then (setq uri (net.uri:parse-uri uri)
		  fresh-uri t))
    
    ; make sure it's an http uri
    (case (or (net.uri:uri-scheme uri) :http)
      (:http nil)
      (:https (setq ssl t))
      (t (error "Can only do client access of http or https uri's, not ~s" uri)))
  
    ; make sure that there's a host
    (if* (null (setq host (net.uri:uri-host uri)))
       then (error "need a host in the client request: ~s" uri))

    (setq scheme-default-port
      (case (or (net.uri:uri-scheme uri) (if* ssl 
					    then :https
					    else :http))
	(:http 80)
	(:https 443)))
    
    ; default the port to what's appropriate for http or https
    (setq port (or (net.uri:uri-port uri) scheme-default-port))
    
    (if* proxy
       then ; sent request through a proxy server
	    (assert (stringp proxy) (proxy) 
	      "proxy value ~s should be a string" proxy)
	    (multiple-value-bind (phost pport)
		(net.aserve::get-host-port proxy)
	      (if* (null phost)
		 then (error "proxy arg should have form \"foo.com\" ~
or \"foo.com:8000\", not ~s" proxy))

		  ;; only create a new socket if none provided
	      (unless sock
			(setq sock (socket:make-socket :remote-host phost
										   :remote-port pport
										   :format :bivalent
										   :type net.aserve::*socket-stream-type*
										   :nodelay t
										   ))))
       else (unless sock				;only create a new socket if none provided
		(setq sock 
			  (socket:make-socket :remote-host host
								  :remote-port port
								  :format :bivalent
								  :type 
								  net.aserve::*socket-stream-type*
								  :nodelay t
					     
								  )))
	   
	    (if* ssl
	       then (setq sock
		      (funcall 'socket::make-ssl-client-stream sock)))
	    )

    #+(and allegro (version>= 6 0))
    (let ((ef (find-external-format external-format)))
      #+(version>= 6) (net.aserve::warn-if-crlf ef)
      (setf (stream-external-format sock) ef))
    
    (if* net.aserve::*watch-for-open-sockets*
       then (schedule-finalization 
	     sock 
	     #'net.aserve::check-for-open-socket-before-gc))
    
    #+io-timeout
    (if* (integerp timeout)
       then (socket:socket-control 
		 sock 
		 :read-timeout timeout
		 :write-timeout timeout))
	    
    
    (if* query
       then (case method
	      ((:get :put)  ; add info the uri
	       ; must not blast a uri we were passed
	       (if* (not fresh-uri)
		  then (setq uri (net.uri:copy-uri uri)))
	       (setf (net.uri:uri-query uri) (query-to-form-urlencoded
					      query
					      :external-format
					      external-format)))
	      (:post 	; make the content
	       (if* content
		  then (error "Can't specify both query ~s and content ~s"
			      query content))
	       (setq content (query-to-form-urlencoded
			      query :external-format external-format)
		     content-type "application/x-www-form-urlencoded"))))
		 
    
    (net.aserve::format-dif :xmit sock "~a ~a ~a~a"
			    (string-upcase (string method))
			    (if* proxy
			       then (net.uri:render-uri uri nil)
			       else (uri-path-etc uri))
			    (string-upcase (string protocol))
			    crlf)

    ; always send a Host header, required for http/1.1 and a good idea
    ; for http/1.0
    (if*  (not (eql scheme-default-port  port))
       then (net.aserve::format-dif :xmit sock "Host: ~a:~a~a" host port crlf)
       else (net.aserve::format-dif :xmit  sock "Host: ~a~a" host crlf))
    
    ; now the headers
    (if* keep-alive
       then (net.aserve::format-dif :xmit
				    sock "Connection: Keep-Alive~a" crlf))

    (if* accept
       then (net.aserve::format-dif :xmit
				    sock "Accept: ~a~a" accept crlf))

    ; content can be a nil, a single vector or a list of vectors.
    ; canonicalize..
    (if* (and content (atom content)) then (setq content (list content)))
    
    (if* content
       then (let ((computed-length 0))
	      (dolist (content-piece content)
		(typecase content-piece
		  ((array character (*))
		   (if* (null content-length)
		      then (incf computed-length 
				 (native-string-sizeof 
				  content-piece
				  :external-format external-format))))
		 
		  ((array (unsigned-byte 8) (*)) 
		   (if* (null content-length)
		      then (incf computed-length (length content-piece))))
		  (t (error "Illegal content array: ~s" content-piece))))
	      
	      (if* (null content-length)
		 then (setq content-length computed-length))))
    
	    
    
    (if* content-length
       then (net.aserve::format-dif :xmit
				    sock "Content-Length: ~s~a" content-length crlf))
    
	    
    (if* cookies 
       then (let ((str (compute-cookie-string uri
					      cookies)))
	      (if* str
		 then (net.aserve::format-dif :xmit
					      sock "Cookie: ~a~a" str crlf))))

    (if* basic-authorization
       then (net.aserve::format-dif :xmit sock "Authorization: Basic ~a~a"
				    (base64-encode
				     (format nil "~a:~a" 
					     (car basic-authorization)
					     (cdr basic-authorization)))
				    crlf))
    
    (if* proxy-basic-authorization
       then (net.aserve::format-dif :xmit sock "Proxy-Authorization: Basic ~a~a"
				    (base64-encode
				     (format nil "~a:~a" 
					     (car proxy-basic-authorization)
					     (cdr proxy-basic-authorization)))
				    crlf))
    
    (if* (and digest-authorization
	      (digest-response digest-authorization))
       then ; put out digest info
	    (net.aserve::format-dif 
	     :xmit sock
	     "Authorization: Digest username=~s, realm=~s, nonce=~s, uri=~s, qop=~a, nc=~a, cnonce=~s, response=~s~@[, opaque=~s~]~a"
	     (digest-username digest-authorization)
	     (digest-realm digest-authorization)
	     (digest-nonce digest-authorization)
	     (digest-uri digest-authorization)
	     (digest-qop digest-authorization)
	     (digest-nonce-count digest-authorization)
	     (digest-cnonce digest-authorization)
	     (digest-response digest-authorization)
	     (digest-opaque digest-authorization)
	     crlf))
	     
				    
				    

    (if* user-agent
       then (if* (stringp user-agent)
	       thenret
	     elseif (eq :aserve user-agent)
	       then (setq user-agent net.aserve::*aserve-version-string*)
	     elseif (eq :netscape user-agent)
	       then (setq user-agent "Mozilla/4.7 [en] (WinNT; U)")
	     elseif (eq :ie user-agent)
	       then (setq user-agent "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)")
	       else (error "Illegal user-agent value: ~s" user-agent))
	    (net.aserve::format-dif :xmit
				    sock "User-Agent: ~a~a" user-agent crlf))

    (if* content-type
       then (net.aserve::format-dif :xmit sock "Content-Type: ~a~a"
				    content-type
				    crlf))
    (if* headers
       then (dolist (header headers)
	      (net.aserve::format-dif :xmit sock "~a: ~a~a" 
				      (car header) (cdr header) crlf)))
    

    (write-string crlf sock)  ; final crlf
    
    ; send out the content if there is any.
    ; this has to be done differently so that if it looks like we're
    ; going to block doing the write we start another process do the
    ; the write.  
    (if* content
       then ; content can be a vector a list of vectors
	    (dolist (cont content)
	      (net.aserve::if-debug-action 
	       :xmit
	       (format net.aserve::*debug-stream*
		       "client sending content of ~d characters/bytes"
		       (length cont)))
	      (write-sequence cont sock)))
    
    
    (force-output sock)
    
    (values
	 (make-instance 'client-request
					:uri uri
					:socket sock
					:cookies cookies
					:method method
					)
	 sock)))


(defun make-http-connection (uri &key 
							 (method  :get)	; :get, :post, ....
							 (protocol  :http/1.1)
							 keep-alive 
							 (accept "*/*") 
							 cookies	; nil or a cookie-jar
							 basic-authorization
							 digest-authorization
							 content
							 content-length 
							 content-type
							 query
							 headers
							 proxy
							 proxy-basic-authorization
							 user-agent
							 (external-format 
							  *default-aserve-external-format*)
							 ssl
							 timeout)
  (multiple-value-bind (_ socket)
	  (make-http-client-request uri
								:method method
								:protocol protocol
								:keep-alive keep-alive 
								:accept accept
								:cookies cookies
								:basic-authorization basic-authorization
								:digest-authorization digest-authorization
								:content content
								:content-length content-length 
								:content-type content-type
								:query query
								:headers headers
								:proxy proxy
								:proxy-basic-authorization proxy-basic-authorization
								:user-agent user-agent
								:external-format external-format 
								:ssl ssl
								:timeout timeout)
	(declare (ignorable _))
	socket))


(defun do-http-request-keep-alive (uri 
			&rest args
			&key 
			(method  :get)
			(protocol  :http/1.1)
			(accept "*/*")
			content
			content-type
			query
			(format :text) ; or :binary
			cookies ; nil or a cookie-jar
			(redirect 5) ; auto redirect if needed
			(redirect-methods '(:get :head))
			basic-authorization  ; (name . password)
			digest-authorization ; digest-authorization object
			keep-alive   ; if true, set con to keep alive
			headers	    ; extra header lines, alist
			proxy	    ; naming proxy server to access through
			proxy-basic-authorization  ; (name . password)
			user-agent
			(external-format *default-aserve-external-format*)
			socket
			ssl		; do an ssl connection
			skip-body ; fcn of request object
			timeout  
			
			;; internal
			recursing-call ; true if we are calling ourself
			)
  
  ;; send an http request and return the result as four values:
  ;; the body, the response code, the headers and the uri 
  (let ((creq (make-http-client-request 
			   uri  
			   :method method
			   :protocol protocol
			   :accept  accept
			   :content content
			   :content-type content-type
			   :query query
			   :cookies cookies
			   :basic-authorization basic-authorization
			   :digest-authorization digest-authorization
			   :keep-alive keep-alive
			   :headers headers
			   :proxy proxy
			   :proxy-basic-authorization proxy-basic-authorization
			   :user-agent user-agent
			   :external-format external-format
			   :socket socket
			   :ssl ssl
			   :timeout timeout
			   )))

	(let (new-location) 
	  
	  (loop
		 (read-client-response-headers creq)
		 ;; if it's a continue, then start the read again
		 (if* (not (eql 100 (client-request-response-code creq)))
			  then (return)))
	  
	  (if* (and (member (client-request-response-code creq)
						'(#.(net.aserve::response-number *response-found*)
						  #.(net.aserve::response-number *response-moved-permanently*)
						  #.(net.aserve::response-number *response-temporary-redirect*)
						  #.(net.aserve::response-number *response-see-other*))
						:test #'eq)
				redirect
				(member method redirect-methods :test #'eq)
				(if* (integerp redirect)
					 then (> redirect 0)
					 else t))			; unrestricted depth
		   then
		   (setq new-location
				 (cdr (assoc :location (client-request-headers creq)
							 :test #'eq))))
	
	  (if* (and digest-authorization
				(equal (client-request-response-code creq)
					   #.(net.aserve::response-number 
						  *response-unauthorized*))
				(not recursing-call))
		   then						   ; compute digest info and retry
		   (if* (compute-digest-authorization 
				 creq digest-authorization)
				then ;; (client-request-close creq)
				(return-from do-http-request-keep-alive
				  (apply #'do-http-request-keep-alive
						 uri
						 :recursing-call t
						 args))))
		  
		  
	  
	  (if* (and (null new-location) 
										; not called when redirecting
				(if* (functionp skip-body)
					 then (funcall skip-body creq)
					 else skip-body))
		   then
		   (return-from do-http-request-keep-alive
			 (values 
			  nil						; no body
			  (client-request-response-code creq)
			  (client-request-headers  creq)
			  (client-request-uri creq))))
	  
	  ;; read the body of the response
	  (let ((atype (if* (eq format :text) 
						then 'character
						else '(unsigned-byte 8)))
			ans
			res
			(start 0)
			(end nil)
			body)
	    
	    (loop
		   (if* (null ans)
				then (setq ans (make-array 1024 :element-type atype)
						   start 0))
		
		   (setq end (client-request-read-sequence ans creq :start start))
		   (if* (zerop end)
				then					; eof
				(return))
		   (if* (eql end 1024)
				then					; filled up
				(push ans res)
				(setq ans nil)
				else (setq start end)))
	    
	    ;; we're out with res containing full arrays and 
	    ;; ans either nil or holding partial data up to but not including
	    ;; index start
	    
	    (if* res
			 then						; multiple items
			 (let* ((total-size (+ (* 1024 (length res)) start))
					(bigarr (make-array total-size :element-type atype)))
			   (let ((sstart 0))
				 (dolist (arr (reverse res))
				   (replace bigarr arr :start1 sstart)
				   (incf sstart (length arr)))
				 (if* ans 
					  then				; final one 
					  (replace bigarr ans :start1 sstart)))
		      
			   (setq body bigarr))
			 else						; only one item
			 (if* (eql 0 start)
				  then					; nothing returned
				  (setq body "")
				  else (setq body (subseq ans 0 start))))
	    
	    (if* new-location
			 then		  ; must do a redirect to get to the real site
			 ;; (client-request-close creq)
			 (apply #'do-http-request-keep-alive
					(net.uri:merge-uris new-location uri)
					:redirect
					(if* (integerp redirect)
						 then (1- redirect)
						 else redirect)
					args)
			 else
			 (values 
			  body
			  (client-request-response-code creq)
			  (client-request-headers  creq)
			  (client-request-uri creq)))))))



(export 'do-http-request-keep-alive :net.aserve.client)
(export 'make-http-connection :net.aserve.client)

;;; remember to (socket:close socket) at the end.

