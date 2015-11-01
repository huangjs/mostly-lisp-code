;;; Copyright: Gary Kings
;;; Copied form his blog:  unClog

;; this lets us write
;;
;; (define-condition record-number-too-large-error
;;     (invalid-record-number-error)
;;   ((record-count :initarg :record-count))
;;   (:report 
;;    (lambda (condition stream)
;;      (format stream 
;; 	     "Record number ~a is too large for this store. Store size is ~a."
;; 	     (slot-value condition 'record-number)
;; 	     (slot-value condition 'record-count)))))
;;
;; as
;;
;; (defcondition record-number-too-large-error
;;     (invalid-record-number-error)
;;     (record-count)
;;     "Record number ~a is too large for this store. Store size is ~a."
;;   record-number record-count)



(defmacro defcondition (name (&rest super-conditions) 
						slot-specs format &body args)
  (flet ((massage-slot (slot-spec)
		   (cond ((atom slot-spec)
				  `(,slot-spec 
					:initarg ,(read-from-string (format nil ":~a" slot-spec))))
				 (t
				  slot-spec)))
		 (massage-format-arg (arg)
		   (cond ((atom arg)
				  `(slot-value condition ',arg)) 
				 (t
				  arg))))
    `(progn
       (export '(,name))
       (define-condition ,name ,super-conditions
		 ,(mapcar #'massage-slot slot-specs)
		 (:report (lambda (condition stream)
					(format stream ,format 
							,@(mapcar #'massage-format-arg args))))))))
  
