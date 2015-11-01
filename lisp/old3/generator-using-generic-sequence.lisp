(common-lisp:defpackage :gen-seq
  (:use :common-lisp :sequence))
(common-lisp:in-package :gen-seq)

;;simple functional iteration
(defmacro luup (name bindings &body loop-body)
  `(labels ((,name ,(mapcar #'car bindings)
              ,@loop-body))
     (,name ,@(mapcar #'cadr bindings))))

;;parses declarations and doc strings out of function bodies.
(eval-when (:load-toplevel :compile-toplevel) 
  (defun fun-body (body)
    (let ((doc nil)
          (dec nil)
          (progn body))
      (loop (cond ((consp progn)
                   (let ((form (car progn)))
                     (cond ((stringp form)
                            (setf doc form
                                  progn (cdr progn)))
                           ((and (consp form) (eq 'declare (car form)))
                            (setf dec (cons form dec)
                                  progn (cdr progn)))
                           (t (return)))))                
                  (t (return))))
      (values progn (nreverse dec) doc))))

(defmacro defglobal (name val)
  (let ((id (gensym)))
    `(progn
       (defvar ,id ,val)
       (define-symbol-macro ,name ,id))))

(defmacro gen-progn (&rest forms)
  (cond ((null forms) 'gen-nil)
        ((null (cdr forms)) 
         (car forms))
        (t (let ((result (gensym)))
             `(bind ,(car forms)
                    #'(lambda (,result)
                        (declare (ignore ,result))
                        (gen-progn ,@(cdr forms))))))))

(defmacro gen-let* (bindings &body body)
  (cond ((null bindings)
         `(gen-progn ,@body))
        (t `(bind ,(cadar bindings)
                  #'(lambda (,(caar bindings))
                      (gen-let*,(cdr bindings) ,@body))))))



;;;generators using a continuation monad as a trampoline
(defclass generator (sb-mop:funcallable-standard-object sequence)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

;;((A -> Answer) -> Answer) -> (continuation A) -- a funcallable tag
(defmacro generator ((current-continuation) &body body)
  (let ((instance (gensym)))
    (multiple-value-bind (body decls) (fun-body body) 
      `(let ((,instance (make-instance 'generator)))
         (sb-mop:set-funcallable-instance-function 
          ,instance 
          #'(lambda (,current-continuation) 
              ,@decls 
              (gen-progn ,@body)))
         ,instance))))

(defmacro defgenerator (name (&rest args) &body body) 
  (multiple-value-bind (body decls doc) (fun-body body)
    `(defun ,name ,args
       ,@(when doc `(,doc))
       ,@decls
       (gen-progn ,@body))))

(defun unit (val)
  (generator (current) (funcall current val)))

(defun bind (generator next)
  (generator (current) 
			 (funcall generator
					  (generator (val) 
								 (funcall (funcall next val)
										  current)))))

(defglobal gen-nil (unit nil))

(defun yield (val)
  (generator (current) (cons current val)))

(defun run (generator)
  (funcall generator #'identity))

(defun next (generator &optional return-value)
  (let ((gen-pair (run generator)))
    (if (consp gen-pair)
        (destructuring-bind (gen . val) gen-pair
          (values (generator (current) 
							 (declare (ignore current))
							 (funcall gen return-value))
                  val))
        (values nil nil))))

(defun contains (val lst &optional (pred #'eql))
  (cond ((null lst) nil)
        ((funcall pred val (car lst)) t)
        (t (contains val (cdr lst) pred))))
              

(defun gen-map (fun &rest generators)
  (labels ((mapr (gens)
			 (destructuring-bind (gens . vals)
				 (reduce #'(lambda (gen rest)
							 (multiple-value-bind (gen val) (next gen)
							   (cons (cons gen (car rest)) 
									 (cons val (cdr rest)))))
						 gens
						 :from-end t
						 :initial-value '(nil))
			   (if (contains nil gens)
				   gen-nil
				   (gen-progn
					(yield (apply fun vals))
					(mapr gens))))))
	(if (contains nil generators)
		gen-nil
		(mapr generators))))

(defmacro do-generator ((var generator) &body body)
  (let ((gen (gensym))
        (do-it (gensym)))
    `(luup ,do-it ((,gen ,generator))
		   (if ,gen 
			   (multiple-value-bind (,gen ,var) (next ,gen)
				 (if ,gen
					 (gen-progn ,@body (,do-it ,gen)) 
					 gen-nil))))))

(defun gen-nth (n gen)
  (cond ((null gen)
         (error "generator is empty"))
        ((zerop n)
         (multiple-value-bind (gen val) (next gen)
           (if gen
               (values val gen)
               (error "generator is empty"))))
        (t (gen-nth (1- n) (next gen)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;new sbcl sequence protocol


(defmethod sequence-length ((seq generator))
  (luup gen-len ((gen seq)
                 (len -1))
		(if (null gen)
			len
			(multiple-value-bind (gen val) (next gen)
			  (declare (ignore val))
			  (gen-len gen (1+ len))))))

(defmethod sequence-elt ((seq generator) index)
  (gen-nth index seq))

(defmethod make-sequence-like ((seq generator) length 
                               &key 
                               (initial-element)
                               (initial-contents nil icp))
  (cond ((and icp (typep initial-contents 'generator))
         (sequence-subseq initial-contents 0))
        (icp (luup gen ((lst (if (listp initial-contents)
                                 initial-contents
                                 (map 'list #'identity initial-contents))))
				   (if (consp lst)
					   (gen-progn (yield (car lst)) 
								  (gen (cdr lst)))
					   gen-nil)))
        (t (luup gen ((n length))
				 (if (< n 1)
					 gen-nil
					 (gen-progn (yield initial-element)
								(gen (1- n))))))))

(defmethod make-simple-sequence-iterator ((seq generator)
                                          &key from-end
										  (start 0)
										  end)
  (when (> start 0)
    (multiple-value-bind (val gen) (gen-nth (1- start) seq)
      (declare (ignore val))
      (setq seq gen)))
  (when end
    (setq seq
          (let ((seq seq))
            (luup gen-subseq ((iter seq)
                              (n start))
                  (cond ((= n end) gen-nil)
                        ((null iter) gen-nil)
                        (t (multiple-value-bind (iter val) (next iter)
                             (if iter
                                 (gen-progn 
                                  (yield val)
                                  (gen-subseq iter (1+ n)))
                                 gen-nil))))))))
  (when from-end
    (setq seq 
          (let ((seq seq))
            (luup gen-rev ((lst (nreverse (map 'list #'identity seq))))
                  (if lst
                      (gen-progn (yield (car lst)) (gen-rev (cdr lst)))
                      gen-nil)))))
  (values seq nil nil))

(defmethod iterator-step ((seq generator) iterator from-end)
  (declare (ignore from-end))
  (next iterator))

(defmethod iterator-endp ((seq generator) iterator limit from-end)
  (declare (ignore limit from-end))
  (or (null iterator) 
      (null (next iterator))))

(defmethod iterator-element ((seq generator) iterator)
  (gen-nth 0 iterator))

(defmethod iterator-copy ((seq generator) iterator)
  iterator)

(defmethod sequence-subseq ((seq generator) start &optional end)
  (with-sequence-iterator (iter lim from-end 
                                i-step i-endp i-ele)
      (seq :start start :end end)
    (luup run-iter ((iter iter))
		  (if (funcall i-endp seq iter lim from-end)
			  gen-nil
			  (gen-progn 
			   (yield (funcall i-ele seq iter))
			   (run-iter (funcall i-step seq iter from-end)))))))
