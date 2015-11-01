;;; exmaple

;;; write function
(defun write-function (result char-size chars stream)
  (with-open-file (f "test.html"
					 :direction :output
					 :if-exists :supersede
					 :if-does-not-exist :create
					 :element-type '(unsigned-byte 8)
					 )
	(write-sequence result stream :start 0 :end (* char-size chars))))

;;; download
(defun curl-test-download ()
  (let ((test (curl:initialize-for-returning-string)))
	;; this is the option setting
	(curl:set-option test :url "http://www.google.co.jp")
	;; this commands the curl to grab the webpage.
	(curl:perform test) 
	(format t "~&~A downloaded. ~%" (length (curl:return-string test)))
	;; save to a file
	(with-open-file (f "test.html"
					   :direction :output
					   :if-exists :supersede
					   :if-does-not-exist :create
					   :element-type '(unsigned-byte 8))
	  ;; (curl:return-string test) is of type (simple-array character (*))
	  (format t "~&~A written. ~%" (length (string-to-octets (curl:return-string test) :null-terminate t)))
	  (write-sequence (string-to-octets (curl:return-string test)) f)
	  ;; this gives the wrong encoding.
	  ;; (format f "~a" (curl:return-string test))
	  )
	(curl:finish test)))

;;; upload
(defun curl-test-upload ()
  (let ((test (curl:initialize-for-returning-string))
		(search-string "hl=en&q=test&btnG=Google+Search"))
	(curl:set-option test :url "http://www.google.com/search")
	(curl:set-option test :postfields search-string)
	(curl:perform test)
	(format t "~&~A downloaded. ~%" (curl:get-information test :size-download))
	(with-open-file (f "test.html"
					   :direction :output
					   :if-exists :supersede
					   :element-type :default)
	  (format f "~a" (curl:return-string test))))
  'done)


;;; curl option code
(defparameter *curl-option-codes*
  `((FILE)
    (WRITEDATA)
    (URL)								; *
    (PORT)								; *
    (PROXY)
    (PROXYUSERPWD)
    (RANGE)
    (INFILE)
    (READDATA)
    (ERRORBUFFER)
    (WRITEFUNCTION)
    (READFUNCTION)
    (TIMEOUT)
    (INFILESIZE)
    (POSTFIELDS)						; *
    (REFERER)							; *
    (FTPPORT)
    (USERAGENT)							; *
    (LOW)
    (LOW)
    (RESUME)
    (COOKIE)							; use :cookies in with-connection-returning-string
    (HTTPHEADER)						; *
    (HTTPPOST)							; *
    (SSLCERT)							; **
    (SSLCERTPASSWD)						; **
    (SSLKEYPASSWD)						; **
    (CRLF)								; *
    (QUOTE)
    (WRITEHEADER)
    (HEADERDATA)
    (COOKIEFILE)						; * 
    (SSLVERSION)
    (TIMECONDITION)
    (TIMEVALUE)
    (CUSTOMREQUEST)
    (STDERR)
    (POSTQUOTE)
    (WRITEINFO)
    (VERBOSE)							; **
    (HEADER)
    (NOPROGRESS)
    (NOBODY)
    (FAILONERROR)
    (UPLOAD)
    (POST)								; **
    (FTPLISTONLY)
    (FTPAPPEND)
    (NETRC)
    (FOLLOWLOCATION)
    (TRANSFERTEXT)
    (PUT)
    (PROGRESSFUNCTION)
    (PROGRESSDATA)
    (AUTOREFERER)
    (PROXYPORT)
    (POSTFIELDSIZE)
    (HTTPPROXYTUNNEL)
    (INTERFACE)
    (KRB4LEVEL)
    (SSL)								; **
    (CAINFO)
    (MAXREDIRS)
    (FILETIME)
    (TELNETOPTIONS)
    (MAXCONNECTS)
    (CLOSEPOLICY)
    (FRESH)
    (FORBID)
    (RANDOM)
    (EGDSOCKET)
    (CONNECTTIMEOUT)
    (HEADERFUNCTION)
    (HTTPGET)							; *
    (SSL)
    (COOKIEJAR)
    (SSL)
    (HTTP)
    (FTP)
    (SSLCERTTYPE)
    (SSLKEY)
    (SSLKEYTYPE)
    (SSLENGINE)
    (SSLENGINE)
    (DNS)
    (DNS)
    (PREQUOTE)
    (DEBUGFUNCTION)
    (DEBUGDATA)
    (COOKIESESSION)
    (CAPATH)
    (BUFFERSIZE)
    (NOSIGNAL)
    (SHARE)
    (PROXYTYPE)
    (ENCODING)
    (PRIVATE)
    (HTTP200ALIASES)
    (UNRESTRICTED)
    (FTP)
    (HTTPAUTH)
    (SSL)
    (SSL)
    (FTP)
    (PROXYAUTH)
    (FTP)
    (IPRESOLVE)
    (MAXFILESIZE)
    (INFILESIZE)
    (RESUME)
    (MAXFILESIZE)
    (NETRC)
    (FTP)
    (POSTFIELDSIZE)
    (TCP)))
