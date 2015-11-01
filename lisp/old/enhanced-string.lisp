(defpackage "enhanced-strings"
  (:use "common-lisp")
  (:export "start")
  (:export "stop"))

(in-package  "enhanced-strings")

(defun parse-enhanced-string (string)
  (loop
	 :with chunks = '()
	 :with args = '()
	 :with start = 0
	 :for pos = (search "${" string :start2 start)
	 :for end = (and pos (search "}" string :start2 pos))
	 :while end
	 :do (progn
		   (push (subseq string start pos) chunks)
		   (multiple-value-bind (form next)
			   (read-from-string string t nil :start (+ 2 pos) :end end)
			 (loop :while (and (< next end)
							   (member (aref string next) '(#\space #\newline)))
				:do (incf next))
			 (unless (= next end)
			   (error "Junk in ~S" (subseq string pos end)))
			 (push form args))
		   (setf start (1+ end)))
	 :finally (progn
				(push (subseq string start) chunks)
				(if (rest chunks)
					(return `(format nil ,(format nil "~{~A~^~~A~}" (nreverse chunks))
									 ,@(nreverse args))))
				(return string))))

(defun reader-macro--enhanced-string (stream dblquote)
  (let ((*readtable* (copy-readtable)))
	(set-macro-character #\" (get-old-macro-character))
	(unread-char dblquote stream)
	(parse-enhanced-string (read stream t nil t))))

(let ((old-macro-character nil))
  (defun start ()
	(unless old-macro-character
	  (setf old-macro-character (get-macro-character #\"))
	  (set-macro-character #\" #'reader-macro--enhanced-string)))
  (defun stop ()
	(when old-macro-character
	  (set-macro-character #\" old-macro-character)
	  (setf old-macro-character nil)))
  (defun get-old-macro-character ()
	old-macro-character))


;; (start)
;; '"this returned string is an enhanced string and the value of x and y will
;; be replaced 'a la ruby' : x=${(* 42 x)}, y =${y}"
;; -->
;; (format nil
;;  "this returned string is an enhanced string and the value of x and y will
;; be replaced 'a la ruby' : x=~a, y =~a"
;;  (* 42 x) y)
