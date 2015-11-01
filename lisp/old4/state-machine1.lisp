;;; example
(defun test () 
  (let (state) 
    (dotimes (i 6) 
      (format t "state=~A~%" state) 
      (machine (state) 
	       (idle 
		(format t "idle entry ~A~%" i) 
		(progn 
		  (format t "idle body ~A~%" i) 
		  (when (= i 2) (go-state two))) 
		(format t "idle exit ~A~%" i)) 
	       (two 
		(format t "two entry ~A~%" i) 
		(progn 
		  (format t "two body ~A~%" i) 
		  (go-state three)) 
		(format t "two exit ~A~%" i)) 
	       (three 
		(format t "three entry ~A~%" i) 
		(progn 
		  (format t "three body ~A~%" i) 
		  (go-state idle)) 
		(format t "thre exit ~A~%" i)))))) 

;;; state machine macro
(defmacro machine (state-var-list default-state &rest state-list) 
  (unless (and (listp state-var-list) 
               (listp default-state) 
               (symbolp (car state-var-list)) 
               (= 4 (length default-state)) 
               (symbolp (car default-state)) 
               (every #'(lambda (x) (listp x)) (cdr default-state)) 
               (every #'(lambda (x) (and (= 4 (length x)) 
                                         (symbolp (car x)) 
                                         (every #'(lambda (y) (listp y)) 
						(cdr x)))) 
                      state-list)) 
    (error "badly formed machine")) 
  (let ((first-time (gensym "first-time-")) 
        (next (gensym "next-")) 
        (state-var (car state-var-list)) 
        (default-state-id (first default-state)) 
        (state-ids (mapcar #'car state-list))) 
    (flet ((gen-name (sym str) 
             (intern (concatenate 'string (symbol-name sym) (string-upcase 
							     str))))) 
      `(macrolet ((go-state (where) 
                    `(progn 
                       (setq ,',next ',,'where) 
                       (go exits)))) 
         (prog ((,first-time (null ,state-var)) 
                ,next) 
	    (when ,first-time 
	      (go ,(gen-name default-state-id "-entry"))) 
	    actions 
	    (case ,state-var 
	      (,default-state-id (go ,(gen-name default-state-id "-action"))) 
	      ,@(mapcar 
		 #'(lambda (s) 
		     `(,s (go ,(gen-name s "-action")))) 
		 state-ids) 
	      (otherwise (return ,state-var))) 
	    entries 
	    (case ,state-var 
	      (,default-state-id (go ,(gen-name default-state-id "-entry"))) 
	      ,@(mapcar 
		 #'(lambda (s) 
		     `(,s (go ,(gen-name s "-entry")))) 
		 state-ids) 
	      (otherwise (return ,state-var))) 
	    exits 
	    (case ,state-var 
	      (,default-state-id (go ,(gen-name default-state-id "-exit"))) 
	      ,@(mapcar 
		 #'(lambda (s) 
		     `(,s (go ,(gen-name s "-exit")))) 
		 state-ids) 
	      (otherwise (return ,state-var))) 
	    ,(gen-name default-state-id "-entry") 
	    (setq ,state-var ',default-state-id) 
	    ,(second default-state) 
	    (unless ,first-time 
	      (return ',state-var)) 
	    ,(gen-name default-state-id "-action") 
	    ,(third default-state) 
	    (return ',state-var) 
	    ,(gen-name default-state-id "-exit") 
	    ,(fourth default-state) 
	    (setq ,state-var ,next) 
	    (go entries) 
	    ,@(apply 'append 
		     (mapcar 
		      #'(lambda (s) 
			  (let ((name (first s)) 
				(entry (second s)) 
				(action (third s)) 
				(exit (fourth s))) 
			    `(,(gen-name name "-entry") 
			       (setq ,state-var ',name) 
			       ,entry 
			       (return ',name) 
			       ,(gen-name name "-action") 
			       ,action 
			       (return ',name) 
			       ,(gen-name name "-exit") 
			       ,exit 
			       (setq ,state-var ,next) 
			       (go entries)))) 
		      state-list))))))) 
