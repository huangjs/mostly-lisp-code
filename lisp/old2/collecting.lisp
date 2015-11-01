#| In 2001, I spent a little too much time thinking about the optimal
interface to a collection utility, as evidenced by this thread on
comp.lang.lisp:
http://groups.google.com/groups?threadm=xcvsnbgix4v.fsf%40famine.OCF.Berkeley.EDU

I'd thought I posted the collection utility I wrote as a result,
but I can't find it anywhere.  So, three years later, here it is.
|#

;;; Copyright 2001, 2002, Thomas F. Burdick
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(defpackage :org.no-carrier.collectors
  (:use :cl)
  (:export #:with-collectors #:collect))

(in-package #:org.no-carrier.collectors)

(defmacro with-collectors ((&rest collectors) &body forms)
  "Evaluate FORMS with collectors established as specified by COLLECTORS.
Each form in COLLECTORS can be a symbol, or a list of the form:
  (var &optional initial-value (fun name))

For each collector, a symbol-macro and a function are defined, allowing one to
use the collector as either a variable or a function. The function takes one
optional argument which is collected, if present.  It returns the collected list
so far.

The initial value of the list being collected into will be INITIAL-VALUE.  The
list may be changed later by SETFing the collector.  Care should be exercised,
however, as this list will be modified.

e.g. 
(with-collectors (c1 c2)
  (dotimes (i 10)
	(if (oddp i)
		(c1 i)
		(c2 i)))
  (list c1 c2))
=>
((1 3 5 7 9) (2 4 6 8 0))

"
  (loop for form in collectors
	 for (head tail fun initvar initform)
	 = (destructuring-bind (var &optional initform (fun var))
		   (if (listp form) form (list form))
		 (list var (gensym "TAIL-") fun (gensym "INIT-") initform))
	 collect `(,initvar ,initform) into inits
	 collect `(,head ,initvar) into vars
	 collect `(,tail (last ,initvar)) into vars
	 collect head into var-names
	 collect `(,fun (&optional (item nil itemp))
					(when itemp
					  (let ((new-cell (list item)))
						(if (consp ,tail)
							(setf (cdr ,tail) new-cell
								  ,tail new-cell)
							(setf ,head new-cell
								  ,tail new-cell))))
					,head) into funs
	 collect fun into fun-names
	 collect `((setf ,fun) (new-list &optional (item nil itemp))
			   (when itemp (,fun item))
			   (setf ,head new-list ,tail (last new-list))
			   ,head) into setfs
	 collect `(setf ,fun) into setf-names
	 collect `(,head (,fun)) into symbol-macros
	 finally (return
			   `(let ,inits
				  (let ,vars
					(flet ,funs
					  (declare (inline ,@fun-names))
					  (flet ,setfs
						(declare (inline ,@setf-names))
						(symbol-macrolet ,symbol-macros
						  (%with-more-collectors (,var-names ,fun-names)
							,@forms)))))))))

 (define-symbol-macro %collector-alist nil)

 (defmacro collect (item &key into &environment env)
   "Collect ITEM into the collector INTO, which should be the collector's name as a variable."
   (let* ((alist (macroexpand-1 '%collector-alist env))
	  (fun (cdr (assoc into alist))))
	 (if fun
	 `(,fun ,item)
	 (error "Attempt to collect into unknown collector ~S." into))))

 (defmacro %with-more-collectors ((vars funs) &body forms &environment env)
   (let ((alist (nconc (mapcar #'cons vars funs)
			   (macroexpand-1 '%collector-alist env))))
	 `(symbol-macrolet ((%collector-alist ,alist))
		,@forms)))
