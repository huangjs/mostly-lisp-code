(defparameter *example-data*
  "SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           x10301MRP220050329..............................
USGE10301TWO          x50214..7050329...............................")

(defparameter *mappings*
  '(("SVCL" service-call
     (4 18 customer-name)
     (19 23 customer-id)
     (24 27 call-type-code)
     (28 35 date-of-call-string))
    ("USGE" usage
     (4 8 customer-id)
     (9 22 customer-name)
     (30 30 cycle)
     (31 36 read-date))))

(defclass service-call ()
  (customer-name customer-id call-type-code date-of-call-string))

(defmethod parse-line-for-class (line (class (eql 'service-call)))
  (let ((object (make-instance class)))
    (loop for (start end slot) in '((4 18 customer-name)
				    (19 23 customer-id)
				    (24 27 call-type-code)
				    (28 35 date-of-call-string))
       do (setf (slot-value object slot)
		(subseq line start (1+ end))))
    object))


(parse-line-for-class "SVCLFOWLER         10101MS0120050313........................." 'service-call)

					;:*=======================
					;:* macros
(defmacro defmapping (name type &body description)
  `(progn
     (defclass ,name ()
       ,(loop for (nil nil slot) in description
	   collect slot))
     (defmethod find-class-for-parser ((type (eql ',(intern type))))
       ',name)
     (defmethod parse-line-for-class (line (class (eql ',name)))
       (let ((object (make-instance class)))
	 (loop for (start end slot) in ',description
	    do (setf (slot-value object slot)
		     (subseq line start (1+ end))))
	 object))))

(defmapping service-call "SVCL"
  (4 18 customer-name)
  (19 23 customer-id)
  (24 27 call-type-code)
  (28 35 date-of-call-string))

(defmapping usage "USGE"
  (4 8 customer-id)
  (9 22 customer-name)
  (30 30 cycle)
  (31 36 read-date))


(parse-line-for-class "SVCLFOWLER         10101MS0120050313........................." 'service-call)
(parse-line-for-class "USGE10301TWO          x50214..7050329..............................." 'usage)


(with-input-from-string (stream *example-data*)
  (loop for line = (read-line stream nil nil)
     while line
     collect (parse-line-for-class line (find-class-for-parser (intern (subseq line 0 4))))))

					;:*=======================
					;:* final version
(defparameter *example-data*
  "SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           x10301MRP220050329..............................
USGE10301TWO          x50214..7050329...............................")

(defmacro defmapping (name type &body description)
  `(progn
     (defclass ,name ()
       ,(loop for (nil nil slot) in description
	   collect slot))
     (defmethod find-class-for-parser ((type (eql ',(intern type))))
       ',name)
     (defmethod parse-line-for-class (line (class (eql ',name)))
       (let ((object (make-instance class)))
	 (loop for (start end slot) in ',description
	    do (setf (slot-value object slot)
		     (subseq line start (1+ end))))
	 object))))

(defmapping service-call "SVCL"
  (4 18 customer-name)
  (19 23 customer-id)
  (24 27 call-type-code)
  (28 35 date-of-call-string))

(defmapping usage "USGE"
  (4 8 customer-id)
  (9 22 customer-name)
  (30 30 cycle)
  (31 36 read-date))

(defun dsl-test ()
  (with-input-from-string (stream *example-data*)
    (loop for line = (read-line stream nil nil)
       while line
       collect (parse-line-for-class line (find-class-for-parser (intern (subseq line 0 4)))))))
