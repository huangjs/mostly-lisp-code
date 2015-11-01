(common-lisp:defpackage :gen-simple
  (:use :common-lisp))
(common-lisp:in-package :gen-simple)

;;;generators using a monad as a trampoline
(defmacro monad-progn (&rest forms)
  (cond ((null forms) nil)
        ((null (cdr forms)) 
         (car forms))
        (t (let ((result (gensym)))
             `(bind ,(car forms)
                    #'(lambda (,result)
                        (declare (ignore ,result))
                        (monad-progn ,@(cdr forms))))))))

(defmacro monad-let* (bindings &body body)
  (cond ((null bindings)
         `(monad-progn ,@body))
        (t `(bind ,(cadar bindings)
                  #'(lambda (,(caar bindings))
                      (monad-let*,(cdr bindings) ,@body))))))

;;((A -> Answer) -> Answer) -> (proc A) -- a funcallable tag
(defmacro proc (args &body body)
  (let ((instance (gensym "PROC_")))
    `(flet ((,instance ,args ,@body))
       (function ,instance))))

(defun unit (val)
  (proc (current) (funcall current val)))

(defun bind (monad-val next)
  (proc (current) 
        (funcall monad-val
                 (lambda (val) 
                   (funcall (funcall next val)
                            current)))))
(defun yield (val)
  (proc (current) (cons current val))) 
 
(defun run (monad-val)
  (funcall monad-val #'identity))

(defvar gen-nil (unit nil))

(defmacro luup (name bindings &body loop-body)
  `(labels ((,name ,(mapcar #'car bindings)
              ,@loop-body))
     (,name ,@(mapcar #'cadr bindings))))

(defmacro as-generator (&body body)
  `(monad-progn 
    ,@body
    gen-nil))

(defun next (generator &optional return-value)
  (let ((gen-pair (run generator)))
    (if (null gen-pair)
        (values nil nil)
        (destructuring-bind (gen . val) gen-pair
          (values (proc (current) 
						(declare (ignore current))
						(funcall gen return-value))
                  val)))))

(defun contains (val lst &optional (pred #'eql))
  (cond ((null lst) nil)
        ((funcall pred val (car lst)) t)
        (t (contains val (cdr lst) pred))))
              

(defun gen-map (fun &rest generators)
  (as-generator
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
					(monad-progn
					 (yield (apply fun vals))
					 (mapr gens))))))
	 (if (contains nil generators)
		 gen-nil
		 (mapr generators)))))

(defun gen-reduce (fun generator init &optional monadic)
  (labels ((monadic-reduce (generator init)
             (if generator
                 (multiple-value-bind (gen val) (next generator)
                   (if gen
                       (bind (funcall fun init val)
                             #'(lambda (new-init)
                                 (monadic-reduce gen new-init)))
                       (unit init)))
                 gen-nil))
           (reducer (generator init)
             (when generator
               (multiple-value-bind (gen val) (next generator)
                 (if gen
                     (reducer gen (funcall fun init val))
                     init)))))
    (if monadic 
        (monadic-reduce generator init)
        (reducer generator init))))
  

(defmacro do-generator ((var generator &optional monadic) &body body)
  (let ((rest (gensym))
        (fun (gensym))
        (mon (gensym)))
    `(let* ((,mon ,monadic)
            (,fun (if ,mon
                      #'(lambda (,rest ,var)
                          (declare (ignore ,rest)) 
                          (monad-progn
						   ,@body))
                      #'(lambda (,rest ,var)
                          (declare (ignore ,rest))
                          ,@body))))
       (gen-reduce ,fun
                   ,generator
                   nil
                   ,mon))))

(defun gen-nth (n gen)
  (cond ((null gen)
         (error "generator is empty"))
        ((zerop n)
         (multiple-value-bind (gen val) (next gen)
           (if gen
               (values val gen)
               (error "generator is empty"))))
        (t (gen-nth (1- n) (next gen)))))


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
;;some generators						;

(defun times (n)
  (as-generator
   (luup times ((m n))
		 (if (< m 1)
			 gen-nil
			 (monad-progn
			  (yield (- n m))
			  (times (1- m)))))))
   

(defun countr (n)
  (as-generator
   (luup countr ((n n))
		 (monad-progn
		  (yield n)
		  (countr (1+ n))))))

(defun fib ()
  (as-generator
   (luup fib ((a 0) 
			  (b 1))
		 (monad-progn
		  (yield b)
		  (fib b (+ a b))))))

(defun fact ()
  (as-generator
   (luup fact ((a 1)
			   (b 1))
		 (monad-progn
		  (yield b)
		  (fact (1+ a) (* a b))))))

(defun list->gen (lst)
  (as-generator
   (luup l-to-gen ((lst lst)) 
		 (if (atom lst)
			 gen-nil
			 (monad-progn
			  (yield (car lst))
			  (l-to-gen (cdr lst)))))))

;;takes an inorder traversal and makes a binary tree out of it
(defun make-tree (seq)
  (let* ((len (length seq))
         (lst-len (ceiling (/ len 2))))
    (cond ((zerop len)
           nil)
          ((= len 1)
           (cons (elt seq 0) (cons nil nil)))
          ((= len 2)
           (cons (elt seq 1) (cons (make-tree (subseq seq 0 1)) nil)))
          (t (cons (elt seq (1- lst-len))
                   (cons (make-tree (subseq seq 0 (1- lst-len)))
                         (make-tree (subseq seq lst-len len))))))))    

(defun inorder (tree)
  (as-generator 
   (luup inorder ((tree tree))
		 (if (null tree)
			 gen-nil
			 (monad-progn
			  (inorder (cadr tree))
			  (yield (car tree))
			  (inorder (cddr tree)))))))

(defun leaves (tree)
  (as-generator
   (luup leaves ((tree tree))
		 (cond ((null tree)
				gen-nil)
			   ((and (null (cadr tree)) (null (cddr tree)))
				(yield (car tree)))
			   (t (monad-progn
				   (leaves (cadr tree))
				   (leaves (cddr tree))))))))

;;the client isn't restricted to monadic or even functional code.
(defun same-fringe (t1 t2 &optional (pred #'eql))
  (let ((gen1 (leaves t1))
        (gen2 (leaves t2))
        (val1 nil)
        (val2 nil))
    (or (eq t1 t2)
        (loop 
           (multiple-value-setq (gen1 val1) (next gen1))
           (multiple-value-setq (gen2 val2) (next gen2))
           (cond ((and (null gen1) (null gen2)) 
                  (return t))
                 ((or (null gen1) (null gen2)) 
                  (return nil))
                 ((funcall pred val1 val2))
                 (t (return nil)))))))                
  
;;test map - reduce
(defun same-fringe2 (t1 t2 &optional (pred #'eql))
  (gen-reduce #'(lambda (rest current)
                  (and rest current))
              (gen-map pred
                       (leaves t1)
                       (leaves t2))
              t))
  
;;we can still use things like catch and throw in non monadic parts of the code
(defun same-fringe3 (t1 t2 &optional (pred #'eql))
  (catch 'null-rv
    (do-generator (x (gen-map #'(lambda (x y)
                                  (if (funcall pred x y)
                                      t
                                      (throw 'null-rv nil)))
                              (leaves t1)
                              (leaves t2)))
      x)))


(let ((file-contents (make-array '(345646) :element-type 'base-char)))
  (with-open-file (in #P"/home/singollo/xfce_log")
	(read-sequence file-contents  in))
  (let ((tree (make-tree file-contents)))
	(time (do-generator (x (inorder tree))
			(format t "~a" x)))))

;;some poker hands
(defvar *deck* '(AS KS QS JS 10S 9S 8S 7S 6S 5S 4S 3S 2S  
                 AH KH QH JH 10H 9H 8H 7H 6H 5H 4H 3H 2H 
                 AC KC QC JC 10C 9C 8C 7C 6C 5C 4C 3C 2C 
                 AD KD QD JD 10D 9D 8D 7D 6D 5D 4D 3D 2D))

(defun comb (items n)
  (as-generator
   (if (zerop n)
	   (yield nil)
	   (luup do-items ((items items))
			 (if (null items)
				 gen-nil
				 (monad-progn
				  (let  ((gen (comb (cdr items) (1- n))))
					(do-generator (cc gen t)
					  (yield (cons (car items) cc))))
				  (do-items (cdr items))))))))
(let ((hands (comb *deck* 5)))
										;...
  )

;;;classic sieve

(defun processor (prime ints)
  (as-generator
   (do-generator (i ints t)
	 (if (zerop (mod i prime))
		 gen-nil
		 (yield i)))))

(defun sieve ()
  (as-generator
   (luup primes ((ints (countr 2)))
		 (multiple-value-bind (ints prime) (next ints)
		   (monad-progn
			(yield prime)
			(primes (processor prime ints)))))))

;;;very slow
(defun mersenne-p (n)
  (luup pow-of-2 ((n (1+ n)))
		(cond ((= n 2) t)
			  ((< n 2) nil)
			  ((zerop (mod n 2))
			   (pow-of-2 (/ n 2)))
			  (t nil))))

(defun mersenne ()
  (as-generator
   (do-generator (p (sieve) t)
	 (if (mersenne-p p)
		 (yield p)
		 gen-nil))))

;;a more traditional/imperative use of generators
(defmacro cstart (&body body)
  ;;should be its own struct
  `(cons (as-generator 
		  gen-nil
		  ,@body) nil))

(defun cstep (corout)
  (and (consp corout)
       (typep (car corout) 'proc)
       (multiple-value-bind (gen val) (next (car corout))
         (setf (car corout) gen)
         val)))

(defun p (x) 
  (print x) 
  (terpri)
  gen-nil)

(defun f ()
  (monad-progn
   (p "b")
   (yield t)
   (p "c")))

(defparameter *c2*
  (cstart
   (p "a")
   (yield t)
   (f)
   (p "d")))

|#
