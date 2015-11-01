#|
general rules

,'	-- no evaluation at the current stage, pass it (therefore the result
	   of previous stage should be a single value) on to the next stage
,	-- evaluate it/them at the current stage, pass the value(s)
	   onto the next stage
,@	-- evaluate it/them at the current stage, slice it/them into pieces,
	   and pass them onto the next stage
,@'	-- no evaluate at the current stage, slice it into pieces
	   (therefore the form must be a list), and pass them onto the next stage
,@'(...)-- bundles up the values returned by the second stage in a list,
	   and then splices the elements of that list into the final result.

NOTE: prefix that performs no evaluation is ,@'(...) but not ,'
NOTE: , | ,@ | ,@'(...) are safe operators

|#


#|
examples:

X		,X		,@X
,',X		,,X		,@,X
,@',X		,,@X		,@,@X
|#

;;; ,@,X means the value of X is an expression whose value is a list
;;; (and is sliced in the final result)
(defmacro foo (X)
  `(defmacro foo2 ()
     `(,@,x)))
(foo (list 'list 1 2))
(foo2)

;;; ,@',X means the value of X is a list that after being sliced, is used
(defmacro foo (X)
  `(defmacro foo2 ()
     `(,@',x)))
(foo (list 1 2))
(foo2)


;;; ,,@X means the value of X should be a list of expressions whose
;;; value is used after being evaluated
(defmacro foo (X)
  `(defmacro foo2 ()
     `(,,@x)))
(foo ('list 1 2))
(foo2)


;;; ,@,@X means the value of X should be a list of expressions each of
;;; which returns a list of values, and they're appended in the final
;;; stage. Finally, the appended form is evaluated and the value is
;;; returned
(defmacro foo (X)
  `(defmacro foo2 ()
     `(,@,@x)))
(foo ((list 'list 1 2) (list 3 4) (list 5 6)))
(foo2)

