#|
OPEN SOURCE LICENSE

This software is Copyright 2002
By Sean Luke 
All Rights Reserved

By using the source code, binary code files, or related data included
in this distribution, you agree to the following terms of usage for
this software distribution. In this license the Author means Sean
Luke (http://www.cs.gmu.edu/~sean/).

The Author hereby grants you a world-wide, royalty-free,
non-exclusive license, subject to third party intellectual property
claims: 

-  to use, reproduce, modify, display, perform, sublicense and
distribute all or any portion of the source code or binary form of
this software or related data with or without mdifications, or as
part of a larger work; and

-  under patents now or hereafter owned or controlled by the Author,
to make, have made, use and sell ("Utilize") all or any portion of
the source code or binary form of this software or related data, but
solely to the extent that any such patent is reasonably necessary to
enable you to Utilize all or any portion of the source code or binary
form of this software or related data, and not to any greater extent
that may be necessary to Utilize further modifications or
combinations. 

In return you agree to the following conditions: 

-  If you redistribute all or any portion of the source code of this
software or related data, it must retain the above copyright notice
and this license and disclaimer. 

-  If you redistribute all or any portion of this code in binary
form, you must include the above copyright notice and this license
and disclaimer in the documentation and/or other materials provided
with the distribution. 

-  You must not use the Author's name to endorse or promote products
derived from this software without the specific prior written
permission of the Author. 

-  If you publish research results derived through the use of all or
any portion of this software or related data, you must make a
reasonable effort to inform the Author and provide the Author with a
reference to your published results. If a reasonable effort fails,
but the Author later requests this reference, you must
provide it. You must also acknowledge the use of this software in
print in the body of your publication, unless your publication is two
pages in length or less in published form. 

-  If you distribute or publish publically-available modifications to
all or any portion of this software or related data, or produce a
product for sale or for license which includes all or any portion of
this software, you must you must make a reasonable effort to inform 
the Author and provide the Author with a reference to
your publically-available modifications or product. If a reasonable
effort fails, but the Author later requests this reference,
you must provide it. You must also acknowledge u se of this software
in a prominent, publically-accessible location in said modifications
or product. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. 
|#

(defpackage :snarf
  (:use :common-lisp-user :common-lisp))
(in-package :snarf)
(export '(new mapobj parent call-if-exists ?call
	      call super super-if-exists ?super slot
	      slot-object mset meth-func func-meth
	      set-original-slot))

(defconstant *snarf-version* 2)

(format t "~%~%Snarf Version ~a~%Copyright 2002 by Sean Luke
This Software has an open source license which requires you
to make certain admissions in your published research or
other projects in order to use the Software.  Please read
the software license in the comments of this file.~%~%" *snarf-version*)
       
(defmacro definline (name &rest stuff)
  "Defines a function and declares it to be inlined"  ;; convenient, no?
  `(progn (declaim (inline ,name))
	  (defun ,name ,@stuff)))


;;; PARENTS
;;; parent   (settable)

(definline parent (object)
  "Returns the parent of an object, else NIL if the object has no parent."
  (cdr object))

(definline (setf parent) (val object) 
  (setf (cdr object) val))


;;; SLOTS
;;; slot     (settable)
;;; slot-object
;;; exists
;;; mapobj
;;; mset
;;; set-original-slot

(let ((retfail (gensym))) ;; failure symbol
  (definline slot-val (slot object)
    "Private"  ;; labels is broken on clisp :-(
    (if (not (consp object)) retfail
      (let (val)
	(dolist (x object retfail)
	  (when (not (hash-table-p x)) (return retfail))
	  (setf val (gethash slot x retfail))
	  (when (not (eq val retfail)) (return val))))))

  (definline slot (slot object &optional (failed-return-value retfail))
    "Looks up a slot in an object.  If the slot exists, its value is returned.
Else if failed-return-value is defined, and the slot does not exist, then
failed-return-value is returned.  Else an unknown-slot error is generated.
This function searches the object, then its parent hierarchy, looking for the slot.
Can be used with SETF.  The setf-version sets the slot directly in the object,
not the parent(s)."
    (let ((val (slot-val slot object)))
      (if (eq val retfail)
	    (if (eq failed-return-value retfail)
		(error (format nil "Unknown slot ~a in object ~a" slot object))
	      failed-return-value)
	  val)))
    (definline exists (slot object)
      "Returns T if a slot exists in an object, or one of its parents, else
returns NIL."
      (not (eq retfail (slot-val slot object)))))

(definline (setf slot) (val slot object &optional failed-return-value)
  (declare (ignore failed-return-value))
  (if (or (not (consp object)) (not (hash-table-p (car object))))
      (error (format nil "Slot ~a cannot be set in object ~a" slot object)))
  (setf (gethash slot (car object)) val))

(let ((retfail (gensym)))
  (definline slot-object (slot object)
    "Searches up the parent chain looking for the object that first contains
the given slot, and returns that object, or NIL if no such object is found."
    (if (not (consp object)) nil
      (let (val (cur object))
	(dolist (x object nil)
	  (when (not (hash-table-p x)) (return nil))
	  (setf val (gethash slot x retfail))
	  (when (not (eq val retfail)) (return cur))
	  (pop cur))))))

(let ((retfail (gensym)))
  (definline set-original-slot (slot object val &optional (failed-return-value retfail))
    "Searches up the parent chain from OBJECT until it finds
the object which defined SLOT, then sets the slot in that object
to VAL.  If there is no such slot in OBJECT or any of its ancestors,
issues an error, or if FAILED-RETURN-VALUE is defined, returns that
value.  Otherwise, returns VAL."
    ;; this is pretty easy to implement
    (let ((orig-object (slot-object slot object)))
      (if orig-object
	  (setf (slot slot orig-object) val)
	(if (eq failed-return-value retfail)
	    (error (format nil "Slot ~a cannot be set in object ~a" slot object))
	  failed-return-value)))))

(defun mapobj (func obj &optional shallow)
  "Maps FUNC over all of the slots in OBJ.  FUNC must take THREE
arguments: (1) the object defining the slot, (2) the slot name,
and (3) the slot value.  If SHALLOW is T, then only slots in OBJ
are mapped.  Otherwise slots in OBJ and all parent objects are
mapped, except for slots in parent objects that cannot be seen
in OBJ because they are overridden.  That is, only one slot
is mapped for a given slot name."
  (if shallow
      (maphash #'(lambda (key val) (funcall func obj key val))
 	       (first obj))
    (let (bag)
      (dolist (x obj)  ;;; gather keys, ugly
	(maphash #'(lambda (key val)
		     (declare (ignore val))
		     (push key bag)) x))
      (mapcar #'(lambda (x)
		  (funcall func (slot-object x obj) x (slot x obj)))
	      (delete-duplicates bag :test #'eq)))))

(let ((o (gensym)))
  (defmacro mset (obj &rest slots)
    "Sets multiple slots in OBJ.  SLOTS is a list of (slotsymbol slotvalue)
pairs.  The object is returned."
  ;;; build the slots  
    (let ((s (mapcar #'(lambda (slot)
			 (when (or (not (listp slot))
				   (not (= (length slot) 2))
				   (not (symbolp (car slot))))
			   (error (format nil "Bad slot definition ~a" slot)))
			 `(setf (slot ',(first slot) ,o) ,(second slot)))
		     slots)))
      `(let ((,o ,obj)) ,@s ,o))))



;;; METHOD CALLS
;;; call-if-exists
;;; call
;;; ?call
;;; super-if-exists
;;; super
;;; ?super
;;; meth-func
;;; func-meth

(let ((retfail (gensym)))
  (definline call-if-exists (method-name object return-if-does-not-exist &rest args)
    "Looks up the slot called METHOD-NAME in object, and calls it as a method,
passing in args.  If the method does not exist, returns return-if-does-not-exist."
    (let ((slot (slot method-name object retfail)))
      (if (not (functionp slot)) return-if-does-not-exist
	  (apply slot object object method-name args)))))

(definline ?call (method-name object &rest args)
  "Looks up the slot called METHOD-NAME in object, and calls it as a method,
passing in args.  If the method does not exist, returns NIL."
  (apply #'call-if-exists method-name object nil args))

(let ((retfail (gensym)))
  (definline call (method-name object &rest args)
    "Looks up the slot called METHODNAME in object, and calls it as a method,
passing in args.  If the method does not exist, an error is signalled."
    (let ((retval (apply #'call-if-exists method-name object retfail args)))
      (if (eq retval retfail)
	  (error (format nil "No such method ~a in object ~a" method-name object))
	retval))))

(defmacro super-if-exists (return-if-does-not-exist &rest args)
  "Calls the method overridden by my method, passing in args.
Returns return-if-does-not-exist if the overridden method does not exist."
  (let ((retfail (gensym)) (method (gensym)) (parent (gensym)))
    `(let ((,parent (parent method-owner)))
       (if (not ,parent)
	   ,return-if-does-not-exist
	 (let ((,method (slot method-name ,parent ',retfail)))
	   (if (not (functionp ,method))
	       ,return-if-does-not-exist
	       (funcall ,method this ,parent method-name ,@args)))))))

(defmacro ?super (&rest args)
  "Calls the method overridden by my method, passing in args.
Returns NIL if the overridden method does not exist."
  `(super-if-exists nil ,@args))

(defmacro super (&rest args)
  "Calls the method overridden by my method, passing in args.
Signals an errorr if the overridden method does not exist."
  (let ((retval (gensym)) (retfail (gensym)))
    `(let ((,retval (super-if-exists ,retfail ,@args)))
       (if (eq ,retval ,retfail)
	   (error (format nil "No such supermethod ~a for object ~a" method-name this))
	 ,retval))))

(defmacro meth ((&rest arguments) &rest body)
  "Produces a method of the given arguments and body."
  `#'(lambda (this method-owner method-name ,@arguments)
       (declare (ignore this method-owner method-name))  ;; in case we don't use them
       ,@body))

(definline meth-func (methodname object)
  "Given a method name and an object, produces a function pointer which will,
when called with some ARGS, will call the method on the object, passing in the ARGS,
and returning the return value of the method." 
  #'(lambda (&rest args)
      (apply #'call methodname object args)))

(definline func-meth (func)
  "Given a function pointer, produces a method which can be added to an object.  Calling
this method will in turn call the function pointer, and return the return value of the function.
This method can be placed into any object, but because it's just a wrapper for a function,
the function has no access to the THIS variable or to any SUPER macros.  In some sense,
this method is thus a ``static method'' a-la Java."
  (meth (&rest arguments)
	(apply func arguments)))




;;; OBJECT CREATION
;;; new

(let ((noargs (gensym)))
  (definline new (&optional (parent nil) (first-init-argument noargs)
			       &rest remaining-arguments)
    "Creates an empty object, optionally setting its parent.  If
any INIT-ARGUMENTS are provided, then INIT is called on the object,
passing in the arguments.  Returns the new argument, plus the return
value of the INIT method (or null if INIT not called) as an optional
return value."
  (let (retval (obj (cons (make-hash-table :test #'eq) parent)))
    (when (not (eq first-init-argument noargs))
      (setf retval (apply #'call 'init obj first-init-argument remaining-arguments)))
    (values obj retval))))


;;; OTHER STUFF INSIDE A METHOD
;;; @  macro
;;;     Forms: @var               Expands to (slot 'var this)
;;;            @(meth ... )       Expands to (call 'meth this ... )
;;; @? macro
;;;     Form:  @?(meth ... )      Expands to (?call 'meth this ... )
;;; #@ macro
;;;     Form:  #@meth             Expands to (meth-func 'meth this)


(set-macro-character
 #\@ #'(lambda (stream char)
	 (declare (ignore char))
	 (let ((first-char (read-char stream)))
	   (cond ((and (char-equal first-char #\?) (char-equal (peek-char nil stream) #\( ))
		  (let ((val (read stream t nil t)))
		    (if (consp val)
			(let ((v1 (first val)) (v2 (rest val)))
			  (cond ((symbolp (first val))
				 `(?call (quote ,v1) this ,@v2))
				(t (error (format nil "Bad method call @?~a" val)))))
		      (error (format nil "Bad method call @?~a" val)))))
		 ((or (char-equal first-char #\newline)
		      (char-equal first-char #\tab)
		      (char-equal first-char #\space)
		      (char-equal first-char #\linefeed)
		      (char-equal first-char #\return))  ;; not all lisps have whitespacep
		  (error (format nil "Whitespace after @")))
		 (t
		  (unread-char first-char stream)  ;; put the ? -- or whatever -- back
		  (let ((val (read stream t nil t)))
		    (cond ((symbolp val)
			   `(slot ',val this))
			  ((consp val)
			   (let ((v1 (first val)) (v2 (rest val)))
			     (cond ((symbolp (first val))
				    `(call (quote ,v1) this ,@v2))
				   (t (error (format nil "Bad method call @~a" val))))))
			  (t (error (format nil "Bad instance variable @~a" val))))))))))

(set-dispatch-macro-character
 #\# #\@ #'(lambda (stream subchar arg)
	     (declare (ignore subchar arg))
	     (let ((val (read stream t nil t)))
	       (if (symbolp val)
		    `(meth-func ,val this)
		 (error (format nil "Must be a symbol in order to funcify: ~a" val))))))


#|
SNARF

Snarf is a very small, simple, typeless, prototype-style (non-class),
single-inheritance object-oriented programming language.  It's not phenominally
fast (its speed is largely dependent on the quality of the hash table implementation
on your system).  But I've found it to be quite handy.  The entire language is
defined in this file.

In Snarf, there are no classes.  There are only objects.  Objects are
simply dictionaries of <symbol, value> pairs.  Objects can have parent objects,
and inherit pairs from them.  The system presently uses single inheritance.

In Snarf parlance, a <symbol, value> pair is called a SLOT.  The name of the slot
is the symbol.  The value of the slot is the value.  The value can be anything.
One particularly useful item to put into the value is a special kind of function,
which we will call a METHOD.  This is a model fairly similar to how Self,
JavaScript, NewtonScript, and Python handle things.   It is *very* different from,
and much simpler than, how matters are handled in CLOS.

Technically, an object is very simple.  It's a cons.  The CAR points to
a hash table which holds the slots stored in the object.  The CDR points
to the object's parent, or to nil if there is no parent.  Thus when you
print an object, it appears to be a list of hash tables.  The first one
is the object's hash tables, the remaining ones are hash tables of ancestors.
That's all there is to it.

[ Because objects are conses holding hash tables and pointing to other conses holding
hash tables, the printed version of an object can get long and ugly depending on
how your lisp implementation prints out hash tables etc.  I could fix this by making
an object be a struct, but I'm concerned that such a beast would be slow and not
easily portable to elisp or scheme.  However, I may change this opinion in the future. ]

Objects are created with the NEW function. To make a blank object with
no parent, you just say:

(new)

To make an object which inherits from another, you say:

(new *parent-obj*)

You can get the parent object of an object with PARENT.
Parents themselves have parents, etc.  Thus an object has
a CHAIN OF ANCESTORS.

(parent *my-obj*)

The PARENT method is settable, but be careful to set it
only to real OBJECTs or to NIL.

(setf (parent *my-obj*) *another-parent-object*)



SLOTS

After you create an object, you can set a slot in it:

(setf (slot 'color *my-obj*) 'blue)

You get a slot like this:

(slot 'color *my-obj*)
...except that this function (unlike the setf version) will look up an
inherited parent slot if *my-obj* doesn't have it.  If there's no
slot or inherited slot called COLOR, all the way up the ancestor chain,
then an error is signalled.

You can tentatively get a slot value, returning a value if there's no
such slot (instead of generating an error):

(slot 'color *myobj* 'return-this-if-failed-to-find)

This form isn't settable -- what would be the point?  To test the
existence of a slot, you say:

(exists 'color *myobj*)

You can even have the system return that object in the ancestor chain
which actually holds the slot you're looking for in its hashtable, else
NIL.

(slot-object 'color *myobj*)

You can also iterate over all the slots in an object.  To print
all the slots in an object and in its ancestors, you can say:

(mapobj #'(lambda (symbol value object)
	    (format t "~%Object ~a has slot ~a with value ~a" object symbol value))
	*myobj*)

This function only prints out one value for a given symbol: thus if an object
overrides ancestor's slots, the ancestor's slots will not be printed in this example.

If you just want to iterate over the immediate slots in an object and not in any
of its ancestors, you can do that too:

(mapobj #'(lambda (symbol value object)
	    (format t "~%Object ~a has slot ~a with value ~a" object symbol value))
	*myobj* t)

To set multiple slots simultaneously in an object, the MSET macro is provided:

(mset *myobj*
      (color 'red)
      (width 42)
      (print-function #'(lambda () (print this))))

This is particularly useful for setting slots in a brand-new object:

(setf *myobj* (mset (new *parent-obj*)
		    (color 'red)
		    (width 42)
		    (print-function #'(lambda () (print this)))))

Sometimes you don't want to set slots in your object, but would prefer to set them
in the parent object from which you can see the slot.  This is most commonly done
to allow multiple objects to "share" a common slot among them.  Setting such a slot
could be done with something along the lines of:

(setf (slot 'the-slot-name (slot-object 'the-slot-name *myobj*)) 'the-new-value)

That's a little ugly, so the following function will do the same thing:

(set-orignal-slot 'the-slot-name *myobj* 'the-new-value)

If there is no such slot, this will generate an error.  Instead you can have it
return an optional value on failure to find the slot:

(set-original-slot 'the-slot-name *myobj* 'the-new-value 'my-failure-symbol)



DEFINING METHODS

Slots can hold many things, but one of the most useful things are METHODS.

Methods are functions which are called in the context of some object.
Methods operate identically to ordinary functions, except that they have access to
special macros and variables which enable them to determine which object they are stored in,
and how to call overridden versions of themselves in parent objects.

Just as functions are created with DEFUN or LAMBDA, Methods are created with the
METH macro.  Here is an example of a method created and stored as a slot in an object.
In this example, the method is just a simple function which takes two arguments
and returns the sum of them.

(setf (slot 'my-function *myobj*)
      (meth (arg1 arg2) (+ arg2 arg2)))

Methods also have access to the object whose context they are in.  Inside a method,
the object is provided with the THIS variable.  Here's an example of a method which
takes two arguments, adds them, sets the object's RESULT slot to the sum, and then
returns the sum:

(setf (slot 'my-function *myobj*)
      (meth (arg1 arg2) (setf (slot 'result this) (+ arg1 arg2))))

A method also can call a method of the same name in a parent object which it had overridden.
To do this, the method can employ the SUPER macro.

(setf (slot 'my-function *myobj*)
      (meth (arg1 arg2)
	    (print (super arg1 45))
	    (+ arg1 arg2)))

If there's no overridden version, calling SUPER will generate an error.
You can tentatively call SUPER-IF-EXISTS, which returns a special
value you provide in the situation that the overridden method doesn't
exist:

(setf (slot 'my-function *myobj*)
      (meth (arg1 arg2)
	    (print (super-if-exists 'NOTHING arg1 45))
	    (+ arg1 arg2)))

There is a simplified version of SUPER-IF-EXISTS called ?SUPER which just
returns NIL if there's no overridden version:

(setf (slot 'my-function *myobj*)
      (meth (arg1 arg2)
	    (print (?super arg1 45))
	    (+ arg1 arg2)))

Because methods are just lambda expressions, they work with closures
just like you'd expect.


HOW METHODS ARE DEFINED UNDERNEATH

A method with ARGS is implemented as a function with three special initial arguments,
then the rest of the arguments are ARGS.  These arguments are THIS, METHOD-OWNER,
and METHOD-NAME.  Thus the following two things are identical:

(meth (x y z) ... )
#'(lambda (this method-owner method-name x y z) ... )

You've already seen the THIS variable.  The METHOD-NAME variable holds the name of the
method.  The METHOD-OWNER variable refers to some object (possibly THIS) whose slot
value gives the method.  METHOD-NAME and METHOD-OWNER are used to handle supermethods
called with the SUPER macro.

In general, while you will find THIS to be useful, you should actively avoid using
METHOD-NAME and METHOD-OWNER ; they exist to let methods do their magic, and will
rarely have much use for you.

At any rate, you should never *change* THIS, METHOD-NAME, and METHOD-OWNER unless you
know exactly what you are doing.



CALLING METHODS

You call a method with the CALL function:

(call 'adder *myobj* 4 7)

Methods do not have access to the THIS or SUPER facilities if you do not execute
them with CALL (for example, if you executed them with FUNCALL or APPLY).

You can also tentatively call the method.  If the method doesn't exist,
you can have a return value sent back instead of an error generated.
To do this, you use the CALL-IF-EXISTS function:

(call-if-exists 'adder *myobj* *return-if-doesn't-exist-value* 4 7)

There is also a simplified version of CALL-IF-EXISTS, called ?CALL, which returns
nil if the method doesn't exist:

(?call 'adder *myobj* 4 7)

Inside a method, the @ read-macro is defined to make self-reference
simpler.  For example, it can be used to call your own methods.  The
following two lines are identical:

@(adder 4 7)
(call 'adder this 4 7)

Inside a method, the @ read-macro can also be used to access slots
in the owning object.  Thus the following two lines are identical:

@foo
(slot 'foo this)

You can also use this in setf:

(setf @foo 4)
(setf (slot 'foo this) 4)

The @? read-macro variant permits you to call your own methods tentatively.
The following two lines are identical:

@?(adder 4 7)
(?call 'adder this 4 7)

Snarf also provides a function which takes a method and an object, and "wraps"
the object into a closure around the method so the method can be used like an
ordinary function in FUNCALL, APPLY, MAPCAR, etc. but still have access to the
THIS and SUPER etc. facilities:

(mapcar
 (meth-func 'adder *my-obj*)
 '(1 2 3 4 5)
 '(6 7 8 9 10))

There is also a shorthand macro #@ for self-referential versions of this.
The following lines are identical:

#@adder
(meth-func 'adder this)

Snarf also lets you wrap a function as a method using the FUNC-METH macro.
Since ordinary functions are NOT methods, they do not have access to the THIS
variable or to the SUPER macros.  Thus if you do something like this:

(mset *my-obj*
      (my-printer (func-meth #'print)))

...this wraps the #'print function as a method that you can stick into a slot on
an object and call with CALL.  In some sense, this function-in-a-method is like
Java's static methods, which also do not have access to any object context even though
they reside in an object.



CONSTRUCTORS

Because objects are created dynamically, not stamped out of a class,
we have not talked about the notion of a CONSTRUCTOR yet.  Indeed, for many prototype-type
languages (like NewtonScript), there is no constructor at all, because what constructors
usually do is set default initial values for things and your parent already has those
things set for you.

But if you must!  If you have a slot called INIT with a method in it, then Snarf can
call this as a constructor if you so desire.  In all other ways, INIT will be treated
as an ordinary method and an ordinary slot.

You can have Snarf call the constructor as part of the NEW function.  If you call:

(new *parent-obj* 4 5 "hello")

...this will create a new object whose parent is *parent-obj*.  It will then call
the INIT method on this object passing in 4, 5, and "hello".  It then returns
two return values.  The primary return value is the new object.  The secondary
return value is the return value of the INIT method.

This is basically the same thing as:

(let ((new-obj (new *parent-obj*)))
  (let ((retval (?call 'init new-obj 4 5 "hello")))
    (values new-obj retval)))

Snarf does not have any notion of finalizers (destructors) at all.
|#
