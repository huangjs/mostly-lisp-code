
;; look into a technique called "trampolining". basically you'd
;; rewrite this:

;;; 1
;;; use library org-no-carrier-tail-funcall, most efficient

(defun len (list)
  (with-tail-call-unit ()
    (labels ((len-iter (list count)
	       (if (null list)
		   count
		   (tail-funcall #'len-iter (cdr list) (1+ count)))))
      (len-iter list 0))))

;;; 2
;;; explicit

(defparameter *limit* 10000000)

(defun func1 ()
  (func2))

(defun func2 ()
  (when (>= *limit* 0)
    (decf *limit*)
    (func1)))

;; as:

(defun func1 ()
  (lambda ()
    (func2)))


(defun func2 ()
  (lambda ()
    (if  (>= *limit* 0)
	 (progn
	   (decf *limit*)
	   (func1))
	 (throw 'done t))))

(defun run-trampolined (func)
  (catch 'done
    (loop for f = func then (funcall f))))

;; When you call this example:

;; (run-trampolined #'func1)

;; It will loop forever, but it won't run out of stack space. now all
;; you have to do is pick the best macros to hide the plumbing with.

;;; another example

(defun len-tramp (list k)
  (lambda ()
    (if (null list)
	(funcall k 0)
	(len-tramp (cdr list) (lambda (v)
				(funcall k (1+ v)))))))

(defun run-tramp (func)
  (catch 'done
    (loop for f = func
       then (funcall f))))

(defun toplevel-k (value)
  (throw 'done value))

(run-tramp (len-tramp (make-list 5) #'toplevel-k))

