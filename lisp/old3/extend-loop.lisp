#|
If "any way" includes hard ways then yes, shadow loop with
your own version that spots your own clauses and bodges them
into a standard loop with ... for x = this then that until
done ...

For example I define a "range" that works like a list of
numbers, though I've called the methods head and tail
instead of car and cdr, and I just store the start and stop
values not all the numbers in between.
|#

(defclass range ()
  ((low :accessor low :initarg :start :initform 0)
   (high :accessor high :initarg :end :initform 10)))

(defmethod head ((object range))
  (if (< (low object)
		 (high object))
	  (low object)
	  (error "Fell off end of range.")))

(defmethod tail ((object range))
  (let ((next (+ (low object) 1)))
	(if (>= next (high object))
		nil
		(make-instance 'range :start next :end (high object)))))

;;; Now we create a version of loop that recognises an "along" clause

(defmacro myloop (&rest clauses)
  (let ((position (position 'along clauses)))
	(if position
		(let ((var (nth (- position 1) clauses))
			  (form (nth (+ position 1) clauses)))
		  (append '(loop)
				  (subseq clauses 0 (- position 1))
				  (build-custom-iterator var form)
				  (subseq clauses (+ position 2))))
		clauses)))

(defun build-custom-iterator (var form)
  (let ((pointer (gensym)))
	`(,pointer = ,form then (tail ,pointer)
               for ,var = (head ,pointer)
               while ,pointer)))

;;; Try it out in conjunction with other loop clauses

(myloop for i across #(fe fo fi fum)
		for j along (make-instance 'range :start 10 :end 13)
		for k in '(a b c)
		collect (list i j k))

;;; => ((FE 10 A) (FO 11 B) (FI 12 C)) 
