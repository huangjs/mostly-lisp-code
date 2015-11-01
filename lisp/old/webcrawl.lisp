
(require :aserve)

(defpackage :wc-user
    (:use :common-lisp :excl :net.aserve.client :net.html.generator))

(in-package :wc-user)


;; (do-http-request uri &key method protocol accept 
;;                           content content-type query format cookies 
;;                           redirect redirect-methods basic-authorization
;;                           digest-authorization
;;                           keep-alive headers proxy user-agent external-format ssl
;;                           skip-body timeout)



(do-http-request "http://www.google.co.jp")

(do-http-request "http://www.google.co.jp/search"
  :query '(("q" . "lisp")
		   ("ie" . "utf-8")
		   ("oe" . "utf-8")))


(defun getpage (page)
  ;;
  ;; demonstrate how to add digest authorization to the
  ;; page retrieval loop that uses client-request-read-sequence
  ;;
  (let ((creq (make-http-client-request page)))
    (read-client-response-headers creq)
    (if* (eq 401 (client-request-response-code creq))
		 then							; try digest authorization
		 (let ((da (make-instance 'digest-authorization
								  :username "joe"
								  :password "secret")))
		   (if* (compute-digest-authorization creq da)
				then	  ; successfully computed digest authorization
				;; values
				;;
				;; end request
				(client-request-close creq)
				;; and create a new one
				(setq creq (make-http-client-request page 
													 :digest-authorization da))
				(read-client-response-headers creq))))
    
    (if* (not (eql 200 (client-request-response-code creq)))
		 then (error "get failed with code ~s"
					 (client-request-response-code creq)))
    
    (let ((buffer (make-array 2048 :element-type 'character)))
      (loop
		 (let ((length (client-request-read-sequence buffer creq)))
		   (if* (zerop length)
				then (return))
	  
		   (format t "got buffer of length ~s:~%~s~%" length
				   (subseq buffer 0 length))))
      
      (client-request-close creq))))


