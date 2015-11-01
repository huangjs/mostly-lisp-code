;;; chapter 2 datatype
(eql 0d0 -0d0)
(equal 0d0 -0d0)
(equalp 0d0 -0d0)

;;; chapter 3 scope and extent
(defun contorted-example (f g x)
  (if (= x 0)
	  (funcall f)						; change to g for comparison
	  (block here
		(+ 5 (contorted-example g
								(lambda ()
								  (return-from here 4))
								(- x 1))))))

(contorted-example nil nil 2)

(defun illegal-examples ()
  (let ((y (block here #'(lambda (z) (return-from here z)))))
	(if (numberp y) y (funcall y 5))))

(illegal-examples)

;;; chapter 4 type specifier
(and integer (satisfies primep))		; primep is a function
(or null (integer 0 *))					; either nil or a non-negative integer

(typep "foo" '(array character))		; specialized to hold character?
(typep #(1 2 3) '(array character))		
(typep "foo" '(array t))
(subtypep 'character (array-element-type #(1 2 3))) ; can hold character?

(setf x (make-array '(100) :element-type 'standard-char))
(typep x '(array character))
(typep x '(array standard-char))
(typep x '(array base-char))
(upgraded-array-element-type 'standard-char)

(function (array &rest fixnum) t) ; function which the 1st arg is an array and the rest args are fixnums, the type of return value is T. This is actually the type declaration of AREF.

(integer 0 7)
(mod 8)
(unsigned-byte 3)

(string 100)
(array 'character (100))
(simple-string 100)
(simple-array string-char (100))

(deftype  mod (n) '(integer 0 (,n)))
(deftype square-matrix (&optional type size)
  "SQUARE-MATRIX includes all square two-dimensional arrays."
  `(array ,type (,size ,size)))
;;; a better one
(defun equidimensional (a)
  (or (< (array-rank a) 2)
	  (apply #'= (array-dimensions a))))
(deftype square-matrix (&optional type size)
  `(and (array ,type (,size ,size))
		(satisfies equidimensional)))

(coerce 7/2 'complex)
(coerce 7/2 '(complex double-float))

;;; chapter 5 program structure
(defun wager (&key ((secret password) nil) amount)
  (format nil "You ~a $~d"
		  (if (eq password 'joe-sent-me) "win" "lose")
		  amount))
(wager :amount 100)
(wager :amount 100 'secret 'joe-sent-me)
(wager :amount 100 :secret 'joe-sent-me) ; => error

((lambda (a &optional (b 3) &rest x &key c (d a))
   (list a b c d x))
 1 6 :d 8 :c 9 :d 10)

(defun array-of-strings (str dims &rest keyword-pairs
						 &key (start 0) end &allow-other-keys)
  (apply #'make-array dims
		 :initial-element (subseq str start end)
		 :allow-other-keys t
		 keyword-pairs))

;;; chapter 6 predicates
lessp			; Y
less-p			; N
standard-char-p	; Y
standard-charp  ; N
string-lessp	; Y
string-less-p	; N

(subtypep '(complex single-float) '(complex float))
(subtypep '(array single-float) '(array float))
(subtypep '(array single-float) '(array short-float))

(consp nil)
(listp nil)
(listp '(1 . 2))

(equalp 3/5 0.6)

;;; chapter 7 control structure
(setf (apply #'aref foo indexes) newvalue)
(shiftf (nth (setq n (+ n 1)) x) 'z)	; eval only once
(prog1 (nth (setq n (+ n 1)) x)
  (setf (nth (setq n (+ n 1)) x) 'z))
rotatef

(setf x (block loser
		  (lambda (x)
			(if (numberp x)
				(* x x)
				(return-from loser nil)))))
(funcall x 3)
(funcall x 'a)

(time (repeating 500000
		(mapcan #'(lambda (x) (and (numberp x) (list x)))
				'(a 1 b c 3 4 d 5))))	; 3 times slower though
(time (repeating 500000
		(remove-if-not #'(lambda (x) (and (numberp x) (list x)))
					   '(a 1 b c 3 4 d 5))))

;;; chapter 8 macro
(defmacro loser (x &optional (a b &rest c) &rest z)
  ...)									; wrong, ambiguous 
(defmacro loser (x &optional ((a b &rest c)) &rest z)
  ...)									; wrong
(defmacro loser (x &optional ((a b &rest c) '(nil nil)) &rest z)
  ...)									; good
(defmacro loser (x &optional ((&optional a b &rest c)) &rest z)
  ...)									; better
;;; then we can call
(loser (car pool) ((+ x 1)))
;;; where
;;; x => (car pool)
;;; a => (+ x 1)
;;; b => nil
;;; c => nil
;;; z => nil

;;; chapter 9 declaration
;;; quote: Declarations that do NOT concern themselves with VARIABLE BINDINGS are pervasive, affecting all code in the body of the special form.
;;; e.g.
(defun foo (x y)
  (declare (notinline floor))
  (floor x)								; floor not inlined
  (let ((z (* x y)))
	(floor y)))							; floor not inlined

;;; when in the initialization, the X3J13 said it's not in the scope of declare
;;; e.g.
(defun nonsense (k x z)
  (foo z x)							; foo not inlined, x z not speical
  (let ((j (foo k x))				  ; foo not inlined, x not special
		(x (* k k)))					; x special
	(declare (inline foo) (special x z))
	(foo x j z)))						; foo inlined, x z special

;;; to declare the initialization form,
(defun foo (x &optional (y (locally (declare (notinline floor)) (floor x))))
  (declare (notinline floor))
  ...)
;;; or, use LOCALLY
(locally (declare (notinline floor))
  (defun foo (x &optional (y (floor x)))
	...))

;;; variable declaration not pervasive
(defun foo (y)
  (declare (type float y))
  (print (type-of y))					; y declared as float
  (let ((y 'a))
	(setf y #c(2 3))))					; y undeclared

;;; good form
(locally
	(declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (defun foo (x &optional (y (abs x)) (z (sqrt y)))
	(bar x y z)))

;;; special
(locally
  (defun hack (thing mod)				; mod might be lexical binding
	(declare (special mod))				; mod is visible to hack1
	(hack1 (car thing)))				; but not that of _thing_

 (defun hack1 (arg)
   (declare (special mod))
   (if (atom arg) mod
	   (cons (hack1 (car arg)) (hack1 (cdr arg))))))

;;; but special declaration does not affect bindings pervasively, (only top-level ones does, like (proclaim '(special x)), defvar, defparameter)
(proclaim '(special x))					; x is always special

(defun example (x y)
  (declare (special y))
  (let ((y 3) (x (* x 2)))				; rebinding y to be lexical
	(print								; 1st y lexical, while
	 (list '+ y (locally (declare (special y)) y))) ;  2nd y special
	(let ((y 4)) 
	  (declare (special y) (special x))	; y is referenced to special binding
	  (print
	   (list '+ y (locally (declare (special y)) y)))
	  (print x))))						; x always special

(example 6 7)

;;; declare function types
(declare (ftype (function () string) foo))

;;; inline/notinline is pervasive

;;; dynamic-extent
;;; some compilers might realize the optimization is possible
(declaim (inline g))
(defun g (x) (declare (dynamic-extent x)) ...)
(defun f () (g (list 1 2 3)))
;;; or
(defun f ()
  (flet ((g (x) (declare (dynamic-extent x))...))
	(g (list 1 2 3))))

;;; stack-allocated rest list
(defun f (&rest x)
  (declare (dynamic-extent x))
  ...)


;;; chapter 10 symbol
(gentemp)


;;; chapter 11 packages
