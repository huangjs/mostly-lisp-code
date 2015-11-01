;; I assume piping as in shell |, right? This worked for me in SBCL, I am 
;; sure CMUCL has something similar... 

(defmacro with-open-pipe ((command args &key input output error io 
								   persist) &body forms) 
  (let ((process (gensym "PROCESS"))) 
    `(let* ((,process (sb-ext:run-program ,command ,args
										  :input :stream
										  :output :stream
										  :error :stream
										  :wait nil )) 
            ,@(when input `((,input (sb-ext:process-input ,process)))) 
            ,@(when output `((,output (sb-ext:process-output ,process)))) 
            ,@(when error `((,error (sb-ext:process-error ,process)))) 
            ,@(when io `((,io (make-two-way-stream 
                               (sb-ext:process-output ,process) 
                               (sb-ext:process-input ,process)))))) 
	   ,(if persist 
            `(progn ,@forms) 
            `(unwind-protect (progn ,@forms) 
			   ,@(when input `((close ,input))) 
			   ,@(when output `((close ,output))) 
			   ,@(when error `((close ,error))) 
			   ,@(when io `((close ,io))) 
			   (sb-ext:process-close ,process) ))))) 


(defun dmm () 
  (with-open-pipe ("/home/tester/vxi11/vxi11chat" '("10.0.0.152" "gpib0,22") :input dmmin :io dmm :persist t)
	(defun dmm-config (string) 
	  (format dmm "~a~%" string) 
	  (force-output dmm)) 
	(defun dmm-ask (string) 
	  (dmm-config string) 
	  (read-line dmm)) 
	(defun dmm-close () 
	  (close dmmin :abort t))))

