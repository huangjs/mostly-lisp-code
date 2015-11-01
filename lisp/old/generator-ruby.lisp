;;; this is a ruby style generator!!
;;; unlike python's generator, if we turn the for loop inside out,
;;; the generator could be replaced with code similar to the below.


(defun yield (&rest args) 
  (declare (special *yield-body*))
  (apply *yield-body* args)) 

(defmacro with-yield (yield-fun &body body) 
  ;; the use of dynamic binding! 
  `(let ((*yield-body* ,yield-fun))    
	 (declare (special *yield-body*))
     ,@body)) 

;;;silly example, but in keeping with the post 

(defun my-counter () 
  (dotimes (x 52) (yield (1+ x)))) 

(with-yield #'(lambda (x) (princ x)) 
  (my-counter)) 

