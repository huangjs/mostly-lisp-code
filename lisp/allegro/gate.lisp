;; an instance of xqueue represents a server and its queue.
;; a gate is used to indicate that data needing to be handled
;; is on the queue.

(defstruct xqueue
  data
  lock
  gate
  process)

;; anyone with access to an xqueue item can add to its work queue by calling
;; xqueue-add. After the data is added, the associated gate is opened
;; (an open gate indicates that data is waiting to be handled).

(defun xqueue-add (xq item)
  (mp:with-process-lock ((xqueue-lock xq))
    (setf (xqueue-data xq) (nconc (xqueue-data xq) (list item)))
    (mp:open-gate (xqueue-gate xq)))
  item)

;; create-xqueue-server starts a server process running and returns
;; the associated xqueue item to which work items can be queued.
;; The server process calls server-function on each element it retrieves
;; from the queue.  When server-function returns :exit, the server
;; process exits.
;;
;; note that the main loop (the process that is set to 
;; the value of (xqueue-process xq)) waits on the gate being open
;; (indicating unhandled data is present) and closes the gate when
;; all available data is handled.

(defun create-xqueue-server (server-function &key (name "Server"))
  (let ((xq (make-xqueue)))
    (setf (xqueue-lock xq) (mp:make-process-lock)
	  (xqueue-gate xq) (mp:make-gate nil)
	  (xqueue-process xq)
	  (mp:process-run-function
	   name
	   #'(lambda (lxq sf)
	       (loop
		  (mp:process-wait "Waiting for data"
				   #'mp:gate-open-p
				   (xqueue-gate lxq))
		  (let (e run)
		    (mp:with-process-lock ((xqueue-lock lxq))
		      (if (null (xqueue-data lxq))
			  (mp:close-gate (xqueue-gate lxq))
			  (progn
			    (setq e (pop (xqueue-data lxq)))
			    (setq run t))))
		    (when (and run (eq :exit (funcall sf e)))
		      (return)))))
	   xq server-function))
    xq))
