;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Fun with Fibonacci numbers.
;;; by Fare Rideau < fare at tunes dot org >
;;;   http://www.cliki.net/Fare%20Rideau
;;; Comments welcome on the announce page:
;;;   http://fare.livejournal.com/59015.html

#|
In this file, we try to illustrate a few ways in which
Lisp allows you to free yourself from limitations to your thinking,
and find elegant solutions to problems that are difficult
to even understand when imprisoned in the mindset of less expressive languages.

1- Launch a Common Lisp evaluator (find a free one on http://cliki.net/)
2- Try the following expressions at the REPL

Using SLIME, ILISP, Zmacs or some other Lisp-friendly interface,
this may be just a matter of hitting a few keystrokes for each form as
you read this file. At worst, you may run a Lisp REPL in a terminal
\(from the middle of nowhere, you may use telnet://prompt.franz.com )
and copy-paste.
http://common-lisp.net/project/slime/

This file assumes you can read and try out Lisp.
For the precise meaning of particular Lisp operators and special forms,
consult your online reference, for instance the CLHS
http://www.lispworks.com/documentation/HyperSpec/index.html
For a tutorial on Lisp, see
http://www.cliki.net/Online%20Tutorial
|#

#| Namespace pollution prevention ; do all our ugly stuff our own package |#
(cl:defpackage :fibonacci (:use #:common-lisp))
(in-package :fibonacci)

#|
Prevent optimization; this may help illustrate some features below.
|#
(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|
Let us introduce the Fibonacci sequence.
The first two terms are 0 and 1 (being literate, we count from 0, not 1);
the remaining of the sequence is defined by each term being
the sum of the two preceding ones.

Here is a straightforward definition of a function returning
the nth element of a sequence, lifted directly from the definition.
Such a definition is often used to illustrate the basics
of recursive function definition.
|#
(defun bogo-fib (n)
  "Bogus implementation of the Fibonacci function."
  (if (< n 2)
      n
      (+ (bogo-fib (- n 1))
	 (bogo-fib (- n 2)))))

#|
Let's check the value of the first elements of that sequence.
The following form returns a list of the first 10 terms.
|#
#+test
(loop for i below 10 collect (bogo-fib i))

#|
This implementation is bogus, because its performance is abysmal.
See bogo-sort in your jargon file for the origin of the name.
http://www.jargon.net/jargonfile/b/bogo-sort.html

For instance, try to compute the 26th term.
|#
#+test
(bogo-fib 25)

#|
This implementation is slow because the leaves of the evaluation tree
are the 1s that are being added (plus quite a few 0s).
Thus, the execution time is proportional to the result,
and as may be known that result grows exponentially with n.
\(For more about Fibonacci numbers themselves,
	 see the list of links at the end of this document.)
|#

#|
In your standard C course, you'll be told that
"iteration is better than recursion", and your teacher will show you
an iterative version of same function, that executes in linear time
\(depending on n).

But in Lisp, you don't even need rewrite your function so as
to achieve linear execution time: just memoize the results.
That is, remember what the function returns,
so that next calls don't have to recompute everything.
This can be done dynamically, while the program is running,
considerably enhancing your performance pattern without restarting.
|#

#|
This makes the Common-Lisp memoization package available to your program,
which depends on this package being installed on your computer.
Before you use this command, you may thus want to
apt-get install cl-memoization
or do the equivalent in your favorite distribution.
In some Linux distributions, you may want to install
a small subset of debian as chroot, then use symlinks
and properly configure your /etc so as to make the good Lisp stuff
available to your usual distribution.
|#
;;; First, ensure asdf is available
(eval-when (:load-toplevel :execute :compile-toplevel)
  (ignore-errors (require :asdf)))
;;; Then, load the memoization package
#+asdf
(eval-when (:load-toplevel :execute :compile-toplevel)
  (ignore-errors
    (asdf:oos 'asdf:load-op :memoization)
    (pushnew :memoization *features*)))

;;; In case you don't have the memoization software package,
;;; here is a short reimplementation of just the functionality we use.
#-memoization
(cl:defpackage #:memoization
  (:use #:common-lisp)
  (:export #:memoize #:unmemoize
	   #:define-memo-function
	   #:memoizing #:memo-lambda))
#-memoization
(in-package :memoization)
#-memoization
(eval-when (:load-toplevel :execute :compile-toplevel)
(defun compute-memoized-function (f h args)
  "the basic helper for computing with a memoized function F,
with a hash-table H, being called with arguments ARGS"
  (multiple-value-bind (results foundp) (gethash args h)
    (if foundp (apply #'values results)
	(let ((results (multiple-value-list (apply f args))))
	  (setf (gethash args h) results)
	  (apply #'values results)))))
(declaim (inline compute-memoized-function))
(defun unmemoize (sym)
  "undoing the memoizing function, return the hash of memoized things so far"
  (let ((r (get sym :original-memoized-function)))
    (when r
      (setf (symbol-function sym) (car r))
      (remprop sym :original-memoized-function)
      (cdr r))))
(defun memoize (sym &optional (h (make-hash-table :test 'equal)))
  "a pretty generic memoizing function to speed things up"
  (unmemoize sym)
  (let ((f (symbol-function sym)))
    (setf (symbol-function sym)
	  #'(lambda (&rest args)
	      (compute-memoized-function f h args))
	  (get sym :original-memoized-function)
	  (cons f h))))
(defmacro define-memo-function (name formals &body body)
  `(progn (defun ,name ,formals ,@body) (memoize ',name)))
(defun memoizing (f)
  (let ((h (make-hash-table :test 'equal)))
    #'(lambda (&rest args)
	(compute-memoized-function f h args))))
(defmacro memo-lambda (formals &body body)
  `(memoizing #'(lambda ,formals ,@body)))
(pushnew :memoization *features*)
)

;;; After this digression, back to computing the Fibonacci numbers.
(in-package :fibonacci)

#|
Now, tell your system to memoize the function:
|#
#+memoization
(memoization:memoize 'bogo-fib)

#|
There you go: performance is now made acceptable
without changing one line of the original code!
This kind of feature can change your life on deployed critical code...
Enhance the running system without changing the code,
and while keeping all the live data!
|#
#+test
(bogo-fib 25)

#|
If your Lisp implementation did optimize the self-calls of function bogo-fib
into static calls, then memoization mightn't work.
Which is why we tried to disable optimization, above.
You may reenable it with
\(declaim (optimize (speed 3) (safety 2) (debug 2)))
|#

#| Tired of writing memoization:... everytime? Use the package. |#
(use-package :memoization)

#| The following is guaranteed to work even with optimization enabled. |#
(eval-when (:load-toplevel :execute :compile-toplevel)
(define-memo-function bigo-fib (n)
  (if (< n 2) n (+ (bigo-fib (- n 1)) (bigo-fib (- n 2))))))

#| Try it out, it's much faster: |#
#+test
(bigo-fib 25)

#|
Why bigo-? Because as n increases, memoizing stores more and more data
in the memoization table. Thus for instance, after the following call,
a table with 1001 elements will have been created:
|#
#+test
(bigo-fib 1000)

#|
Notice how big that number is? Hey, you can't express it with C integers!
Exercise: can you tell for which n the n+1st term fib(n) will be too big
for a 32-, 64- or 128- bit signed or unsigned C integer?
Not that much really, is it?
|#
(defun index-max-fib-below-2** (bits)
  (loop
     with m = (expt 2 bits)
     for i from 0
     when (>= (bigo-fib i) m) do (return (1- i))))
(loop
  for bits in '(31 32 63 64 127 128)
  for n = (index-max-fib-below-2** bits)
  do (format t "fib(~A)=~A is the biggest Fibonacci number below 2**~A~%"
	     n (bigo-fib n) bits))

#|
Note that since C integers overflow quickly, a table that contains
all the Fibonacci numbers that C could compute will be really small.
Who needs a linear version in C, when Lisp can easily give you
a constant-time version for the same input set?
And what about C overflowing not just quickly but also silently
\(and Pascal throwing an exception),
when Lisp will instead continue gracefully with bignums?

In many problems, static languages such as C force you
to choose static limitations, which makes your life a hell,
whether you accept these limitations and are eventually hit
by a real case that is out-of-bounds in your deployed software,
with catastrophic results, or whether you try to go around these
limitations, at enormous costs in terms of coding libraries
with clumsy use syntax and lots of defensive implementation tests.

Note that even remaining in C grounds, if a C programmer were to implement
a table, he'd have to painfully write each of the numbers in a constant table ;
the Lisp programmer could just have the computation done at compile-time
by a macro.
|#
(defun fib-with-C-limitations-faster-than-C (n)
  (check-type n (integer 0 #.(index-max-fib-below-2** 32)))
  (aref #.(apply 'vector
		 (loop for i from 0 to #.(index-max-fib-below-2** 32)
		   collect (bigo-fib i)))
	n))

#|
Writing constants by hand implies human errors that are not easily
found by proof-reading the source, and is hard to maintain in cases when the
constant table actually depends on parameters that may evolve,
such as the maximum number of bits here, or parameters of some more complex
formula, in a real-life program that involves such tables.
Compile-time evaluation solves these problems of software maintainability.
Also, as opposed to C++ that allows for compile-time evaluation in a
completely different and difficult to debug language, its template language,
Lisp allows for compile-time evaluation in the very same language,
which means that you can directly reuse the code you have developped
and debugged using your usual programming environment.
|#

#|
Now, if you don't like tables, not even small tables, or if you really want
the very same algorithm as you'd write in C, you can have it in Lisp, too.
And we can also locally reenable optimization, if you like it.
|#
(defun bubble-fib (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (check-type n fixnum)
  (loop repeat n
	 with p = 0 with q = 1
	 do (psetq p q
			   q (+ p q))
	 finally (return p)))

#| Test it! |#
#+test
(loop for i below 50 collect (bubble-fib i))

#|
There you have your Lisp version of that C program.
Note that even as you transcribe your algorithm from C,
psetq frees you from the hassle of introducing an ad-hoc temporary variable
as you are forced to do in C, while the Lisp arithmetics lets you
go beyond what where you'd get in C. More results for less work.

However, if you think this algorithm is clever, you're quite wrong.
Actually, it's quite a naive algorithm, and is to computing
the Fibonacci sequence what bubble-sort is to sorting,
as contrasted to bogo-fib above being analogous to bogo-sort.
http://www.jargon.net/jargonfile/n/naive.html
http://www.jargon.net/jargonfile/b/bubblesort.html

If you don't realize how bad that algorithm is,
it is only because of the limitations C imposes upon you!
Indeed, it is possible to compute Fibonacci numbers
in logarithmic time rather than linear time.
\(actually, in polylog time -- a time bounded
		   by a polynomial of the logarithm of n),
but you will never find out unless you can compute bigger numbers
than C allows to express, anyhow!
|#

#|
To explain the polylog algorithm, first,
realize how the loop of the bubble-fib function
is iterating this basic operation:
|#
(defun fib-step (p q) (values q (+ p q)))

#| Try it out, and notice the Lisp function returning multiple values: |#
#+test
(fib-step 2 3)

#|
Multiple values are very nice. They make things much more regular,
and allow to express and manipulate concepts unthinkable without them.
Of course, the Turing Theorem says you can ultimately always
do it with a very clumsy syntax and abysmal performance
in whichever your favorite language is -- by Greenspunning.
http://www.abelard.org/turpap2/tp2-ie.asp
http://c2.com/cgi-bin/wiki?GreenSpunning

Consider the following simple "design pattern" of iteration:
|#

(defun iter-1 (f n &rest args)
  (if (zerop n) (apply #'values args)
      (apply #'iter-1 f (- n 1)
	     (multiple-value-list (apply f args)))))

#+test
(iter-1 'fib-step 50 0 1)

#|
Informal "design patterns" are only for inexpressive languages.
In Lisp, you can always express the "design pattern" formally,
through a function or a macro.
The only required "design pattern" in Lisp is:
"evolve the language using the language itself".
No half-assed informal descriptions of repetitive patterns needed;
if you can actually identify and express a redundancy using English language,
you can also code a formal function or macro to get rid of it
\(and yes, just like your account of the "pattern" in English may evolve,
	 your formal functions, macros and combination thereof may evolve).
http://www.cliki.net/Design%20Patterns
http://sbcl-internals.cliki.net/oaoom
|#

#| Try it with a larger value |#
#+test
(iter-1 'fib-step 50000 0 1)

#|
Ran out of stack? May happen in some systems.
Well, write an iterative version of same function.
|#
(defun iter-1.1 (f n &rest args)
  (loop for vals = args then (multiple-value-list (apply f vals))
     repeat n
     finally (return (apply #'values vals))))

#+test
(iter-1.1 'fib-step 50000 0 1)

#|
Got annoying messages from your GC? You can turn them off.
Here's the way it's done with CMUCL.
YMMV if you use another implementation.
|#
#+cmu (setf extensions:*gc-verbose* nil)

#|
Notice the #+ for conditional evaluation?
Allows to make code that's portable accross implementations.
No such horror as a C preprocessor required.
Did I tell that Lisp Macros were quite unlike CPP macros?
http://www.cliki.net/On%20Lisp
|#

#| OK, so we may use our iterator to refactor bubble-fib |#
(defun bubble-fib-2 (n)
  (values (iter-1.1 'fib-step n 0 1)))

#|
The (values ...) form here is a colloquialism to keep
only the first value being returned. It's equivalent to (nth-value 0 ...).
For the second value, we'd use (nth-value 1 ...), etc.
|#

#+test
(loop for i below 50 collect (bubble-fib-2 i))


#|
Now that we have refactored the Fibonacci function,
new horizons are opened as to how to make it faster.
Indeed, couldn't that iteration be sped up?
After all, we know a lot of functions the iteration of which can be sped up.

To begin with, let's express the notion of composing functions,
so as to refactor our programs further.
|#

(defun compose/2 (f g)
  (lambda (&rest args) (apply f (multiple-value-list (apply g args)))))

#+test
(funcall (compose/2 '1+ '1+) 3)


#|
See how it is a "higher-order function": a function that takes functions
as parameters and returns a function as a result.
The paradigm of using higher-order functions to express abstract computations
is called functional programming, and languages that can express this paradigm
in a seamless way are called functional programming languages.
However, note that while Lisp can express this paradigm
and thus be viewed as a functional programming language,
unlike mere "functional programming languages", it can also express
any paradigm you actually want it to express:
object programming, logic programming, relational queries, etc.
|#

#|
One thing difficult to express fully in most
"static" functional programming languages
is composition of a list of _arbitrary_ functions.
Of course, when a program is done, you find that
things are never quite arbitrary, a posteriori.
But it is a pain in the ass to have to constantly refine a concept
because you can only express but restricted versions of it,
short of Greenspunning.
|#

(defun compose (&rest functions)
  (if (null functions) #'values
      (compose/2 (car functions) (apply 'compose (cdr functions)))))

(defun 2* (n)
  (+ n n))

#+test
(funcall (compose '2* '1+ '2*) 3)

#| We can use this refactoring to compute Fibonacci numbers: |#

(defun iter-2 (f n &rest args)
  (apply (apply 'compose (loop for i below n collect f)) args))

#+test
(iter-2 'fib-step 50 0 1)

#|
Now, consider that there are many functions the iteration of which
can be done quite fast, with a proper algorithm:
for instance, iterating function 1+ (that increments a number),
or the function (LAMBDA (X) (+ A X)) that adds A to its argument;
the Nth iterate of the former is (LAMBDA (X) (+ N X)),
and the Nth iterate of the latter is (LAMBDA (X) (+ (* N A) X)),
similarly, the Nth iterate of (LAMBDA (X) (* A X)) is
\(LAMBDA (X) (* (EXPT A N) X)) -- multiplying by the Nth power of A.
\(The astute reader will notice and correct an off-by-one bug
	 in the above statement. Darn the English language.)

The key if you know a bit of algebra, is that in a (semi-)group structure,
composition with an element is an associative operator.
Thus, you can refactor the iterated composition in terms of
the element being composed with itself a number of times
equal to a power of two.

There goes the classical polylog algorithm for computing
the Nth power of a number (or of anything, actually):
|#

(defun nat-expt (base exponent &optional (factor 1))
  (if (zerop exponent) factor
      (nat-expt (* base base)
		(ash exponent -1)
		(if (oddp exponent) (* base factor) factor))))

#+test
(nat-expt 3 5)

#|
This works for multiplication, too -- and inside your computer,
multiplications are indeed computed according to some simple or farfetched
variant of this algorithm.
|#
(defun nat-mult (base multiplier &optional (sum 0))
  (if (zerop multiplier) sum
      (nat-mult (+ base base)
		(ash multiplier -1)
		(if (oddp multiplier) (+ base sum) sum))))

#+test
(nat-mult 8 7)

#|
And of course, this works for function iteration:
the function below builds a nice function of logarithmic size
that computes the Nth iterate of the base function.
|#
(defun fun-expt (function iterations &optional (seed #'values))
  (if (zerop iterations) seed
      (fun-expt (compose/2 function function)
		(ash iterations -1)
		(if (oddp iterations) (compose function seed) seed))))

(defun iter-3 (f n &rest args)
  (apply (fun-expt f n) args))

#+test
(iter-3 'fib-step 50 0 1)

#|
Now, let's apply it to compute large Fibonacci numbers.
Do we have a speed up as compared to previous versions?
|#

#+test
(iter-3 'fib-step 50000 0 1)

#|
No, there wasn't a sensible speed up. Darn. Why so?
Because while the iterated function is logarithmic in size,
the total number of calls is still equal to the input parameter n!
Indeed, the only way that the function iteration process sees the base function
is by applying it. Composing functions with compose/2 makes for a tight pack,
but since the composed functions are opaque, it cannot optimize the result.
The only thing that was gained is that the depth of the call-graph
is logarithmic instead of linear, allowing the program to run with
a smaller stack size than a plain recursive function
without tail-call optimization
\(but then, the plain iterative version runs with constant stack).

So that the algorithm be really better, we need to be able to efficiently
combine the functions into something synthetic.
If we had a Sufficiently Smart Compiler (SSC), we could use
\(compile (compose/2 function function)) instead of
\(compose/2 function function), and let it optimize everything.
But then, with a *real* SSC, we could write bogo-fib
and let it optimize everything for us.
http://c2.com/cgi/wiki?SufficientlySmartCompiler
And while we're at it, with the *ultimate* SSC, we could just DWIM.
http://www.jargon.net/jargonfile/d/DWIM.html
So the real interesting question is "What would a SSC do?"
http://home.usadatanet.net/~paruby/06_boitano.htm

Now, not being able to rely on an external oracle to do the job
of simplifying composition, we need to open up
the implementation of the function composition,
at least for the particular kind of functions we compose.
We will thus have to represent these particular functions
as user-manipulable objects instead of opaque apply-only functions.

We thus get the following generic algorithm:
|#

(defun generic-expt (composer function iterations seed)
  (if (zerop iterations) seed
      (generic-expt composer
		    (funcall composer function function)
		    (ash iterations -1)
		    (if (oddp iterations)
			(funcall composer function seed)
			seed))))

#+test
(funcall (generic-expt 'compose/2 '1+ 3 #'values) 4)

#|
Actually, by making the applier visible, we can also
define the generic algorithm for iterating on a value:
|#

(defun generic-iterator (self-composer applier seed function iterations)
  (if (zerop iterations) seed
      (generic-iterator self-composer applier
			(if (oddp iterations)
			    (funcall applier function seed)
			    seed)
			(funcall self-composer function)
			(ash iterations -1))))

#+test
(generic-iterator #'(lambda (f) (compose/2 f f)) 'funcall 4 '1+ 1000)

#|
Of course, if we don't trust the compiler to optimize tail-calls
into iteration, we can write an iterative version.
\(Though there is no need for a SSC for such an optimization --
		many compilers do it already if optimizations are not disabled.
		Actually, the Scheme dialect of Lisp mandates such "optimization"
		as an implicitly required behaviour of any compiler or evaluator.)
|#

(defun generic-iterator-loop (self-composer applier seed function iterations)
  (loop until (zerop iterations)
    do
    (psetq seed (if (oddp iterations) (funcall applier function seed) seed)
	   function (funcall self-composer function)
	   iterations (ash iterations -1))
    finally (return seed)))

#+test
(generic-iterator-loop #'(lambda (f) (compose/2 f f)) 'funcall 4 '1+ 1000)

#|
Now, the fib-step function iterated in the Fibonacci sequence
is a linear function! And if you remember linear algebra,
we can conveniently represent linear functions as matrices.
http://hverrill.net/courses/linalg/

Here is a simplistic implementation of matrices of arbitrary size in Lisp.
It's not meant to be efficient, only to fit in little space.
We give a few extra functions, to illustrate the power of Lisp
and functional programming, but little comments.
|#
(defun list->matrix (M)
  M)
(defun list->vector (V)
  V)
(defun nth-matrix-vector (n M)
  (nth n M))
(defun nth-vector-element (n V)
  (nth n V))
(defun matrix->vector (M)
  (unless (= 1 (matrix-columns M))
    (error "Matrix has more than one columns"))
  (car M))
(defun vector-list->matrix (list)
  list)
(defun vector->matrix (V)
  (vector-list->matrix (list V)))
(defun matrix-apply (M V)
  (matrix->vector (matrix-compose-1 M (vector->matrix V))))
(defun matrix-rows (M)
  (length (car M)))
(defun matrix-columns (M)
  (length M))
(defun matrix-element (M i j)
  (nth-vector-element i (nth-matrix-vector j M)))
(defun matrix-compose-1 (M N)
  (unless (= (matrix-columns M) (matrix-rows N))
    (error "Matrix dimensions do not match!"))
  (vector-list->matrix
   (loop for k below (matrix-columns N)
	 collect
	 (loop for i below (matrix-rows M)
	       collect
	       (list->vector
		(loop for j below (matrix-columns M)
		      sum (* (matrix-element M i j)
			     (matrix-element N j k))))))))

#+test #+test #+test #+test
(matrix-compose-1 '((-1 0 1)) '((1) (2) (3) (4)))
(matrix-compose-1 '((2) (3)) '((4 5)))
(matrix-compose-1 '((1 2) (3 4)) '((1 -1) (1 0)))
(matrix-apply '((2) (3)) '(4 5))

#|
Exercise: rewrite matrix-compose using mapcar and such,
so as to avoid walking matrixes with matrix-element.

Solution below.
|#


(defun matrix-transpose (M)
  (apply 'mapcar 'list M))

#+test
(matrix-transpose '((2 3)(4 5)))

(defun scalar-product (v w)
  (reduce #'+ (mapcar #'* v w)))
#+test
(scalar-product '(1 2 3) '(4 5 6))

(defun matrix-compose (m n)
  (mapcar #'(lambda (ncol)
	      (mapcar #'(lambda (mrow)
                          (scalar-product mrow ncol))
		      (matrix-transpose m)))
	      n))

(defun matrix-compose-2 (m n)
  (let ((tm (matrix-transpose m))
	(xn (mapcar #'(lambda (ncol) #'(lambda (mrow)
	                                  (scalar-product mrow ncol))) n)))
	(mapcar #'(lambda (xncol) (mapcar xncol tm)) xn)))


#+test #+test #+test #+test
(matrix-compose '((-1 0 1)) '((1) (2) (3) (4)))
(matrix-compose '((2) (3)) '((4 5)))
(matrix-compose '((1 2) (3 4)) '((1 -1) (1 0)))
(matrix-compose-2 '((1 2) (3 4)) '((1 -1) (1 0)))

(defun kronecker-delta (i j)
  (if (= i j) 1 0))
(defun identity-matrix (n)
  (loop for i below n collect
	(loop for j below n collect
	      (kronecker-delta i j))))

#+test
(identity-matrix 3)

(defun vector-add (V W)
  (mapcar '+ V W))

#+test
(vector-add '(1 2 3) '(4 5 6))

(defun matrix-add (M N)
  (mapcar 'vector-add M N))

#+test
(matrix-add '((0 1) (2 3) (4 5)) '((6 7) (8 9) (10 11)))

(defun vector-scale (l V)
  (mapcar (lambda (x) (* l x)) V))

(defun matrix-scale (l M)
  (mapcar (lambda (V) (vector-scale l V)) M))

#+test
(matrix-scale -1 (identity-matrix 3))

(defun matrix-trace (M)
  (unless (= (matrix-columns M) (matrix-rows M))
    (error "Not a square matrix!"))
  (loop for i below (matrix-columns M)
	sum (matrix-element M i i)))

#+test
(matrix-trace (identity-matrix 7))

#| Now, here is matrix exponentiation as a simple application
of our generic exponentiation above. |#

(defun matrix-expt (m n &optional (seed (identity-matrix (matrix-columns m))))
  (generic-expt 'matrix-compose m n seed))

#+test #+test
(matrix-expt '((1 0) (1 1)) 50)
(matrix-expt '((0 1) (1 1)) 50)

#|
And here is a fast polylog-time implementation of the Fibonacci function,
using the above matrix exponentiation.
Notice that applying to vector (0 1) and taking the first value
is same as extracting the right element of the matrix. So we do.
|#

(defun fast-fib (n)
  (matrix-element (matrix-expt '((0 1) (1 1)) n) 0 1))

#| Check that it returns the same results as above implementations. |#

#+test
(loop for i below 50 collect (fast-fib i))

#| Is it really faster? |#
#+test
(fast-fib 50000)

#| Yes! Now, we can compute numbers previously unreachable. |#
#+test
(fast-fib 100000)

#|
Exercise: for faster matrix-composition, write macros that expand into
optimized operations that represent matrixes as multiple values.
|#


#|
So you want a real fast implementation of Fibonacci?
Well, you can hand-inline all the computations in generic-iterator-loop;
and instead of using generic matrix composition, you can special-case it
for the algebra of 2*2 matrices of the form ((,a ,b) (,b ,(+ a b))),
like all Fibonacci matrices are.
Thus, you obtain the following optimized function,
that to me is about 3 times faster than the above "fast-fib".
|#
(defun very-fast-fib (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (check-type n fixnum)
  (let ((a 0) (b 1)  ;;; the matrix to exponentiate
	(p 0) (q 1)) ;;; the seed vector to which to apply it
    (loop
      for c = (+ a b)
      until (zerop n)
      when (oddp n)
      do (psetq p (+ (* a p) (* b q))
		q (+ (* b p) (* c q))) ;;; applying the current matrix
      do (psetq n (ash n -1) ;;; halving the exponent
		a (+ (* a a) (* b b))
		b (* b (+ a c))) ;;; squaring the current matrix
      finally (return p))))

#| Always check your implementation... |#
#+test
(loop for i below 50 collect (very-fast-fib i))

#|
Now, if you want to get timing values, dropping the display-cluttering result,
this'll do...
|#

#+test #+test
(time (nth-value 1 (fast-fib 100000)))
(time (nth-value 1 (very-fast-fib 100000)))

#|
Fun benchmark:
\(very-fast-fib 50000) takes 18.190 seconds on my MacIvory 3,
0.18 seconds on my Jornada 820 with clisp,
and about 0.05 seconds on a 3GHz Xeon with CMUCL.
Of course, this is all very dependent on
the algorithm used to multiply verybignums,
and unlike specialized algebra packages, Lisp implementations
do not usually have a top-of-the-line implementation for such big numbers.
You might want to hack your own (and contribute it back to SBCL/CMUCL?).
Ouch.
|#

#|
You may use time and similar functions to benchmark your code.
You'll find that the break-even for the more complex algorithm
is below the horizon of small integer results -- and that for smaller numbers,
a table lookup beats any linear time implementation,
in space as well as in time!

Now, you'll tell me that any language can express the optimized very-fast-fib
implementation of Fibonacci numbers. Sure. Any plain old Turing Tar-Pit could.
http://wombat.doc.ic.ac.uk/foldoc/foldoc.cgi?Turing+tar-pit
http://www.geocities.com/ResearchTriangle/Station/2266/tarpit/
But could any language have expressed all the intermediate steps and concepts
that have been used while refactoring the function?
Could any language provide you with the mind tools necessary
to evolve your program?
|#

#|
Is such an algorithm relevant for something else than Fibonacci numbers?
Well, sure. In some computer simulations of "universes" that can be seen
as kind of "linear" or "semi-linear" from an algebraic point of view,
you can speed up "time-warp" with this algorithm.

And even when this particular algorithm doesn't apply, well,
the kind of high-level refactoring that you can do in Lisp
may allow you to find new faster implementations for your programs,
reusing known high-level algorithms found in available literature ;
or it may allow you to find new applications to your programs,
by changing the representations it uses, that make actually possible
what was previously unreachably complex computations.

For instance, transforming iteration into composition
is an instrumental concept in the parallelization of sequential algorithms.
When a function is being composed with itself, it may be computed once
by one processor only, and the result used twice ;
but when two different functions are being composed, they may be computed
in parallel each by a processor, and then the results can be composed by one.
|#

#|
Oh, and as for people who know this formula obtained by solving
the linear recurrence relation in the Fibonacci sequence,
a simple algorithm for computing the Nth Fibonacci number is as follows:
|#
(defvar *phi* (/ (+ 1 (sqrt 5.0d0)) 2))
(defun rfib (n)
   (/ (- (expt *phi* n) (expt (- *phi*) (- n))) (sqrt 5)))
#+test
(loop for i below 50 collect (rfib i))
#|
However, though this algorithm might be polylog (or even constant time),
the precision it yields is also quite bounded.
|#
#+test
(rfib 1000)
#|
For those with dim memories of their lessons in elementary algebra,
the secret of the above formula is that phi, also known as the Golden Ratio,
is the larger eigenvalue of the companion matrix M=((0 1)(1 1))
to the Fibonacci recursive equation un+2 = un+1 + un.
phi satisfies phi^2=1+phi. The other eigenvalue phibar is 1-phi,
and it also the opposite of the inverse of phi, -1/phi.
Note that M is also the matrix of multiplication by phi
in the base (1, phi) of Q[phi], and that
the sequence of term (vector (F n) (* phi (F (1+ n)))) is
a geometric sequence of ratio phi.
The three previous sentences remain true when replacing
all instances of phi by phibar and instances of phibar by phi.
Sequences satisfying the Fibonacci recursive equation
are a two-dimensional vector space, and a base of them
is the geometric sequences of powers of phi and powers of phibar
-- Fibonacci sequences are thus linear combinations
of these two geometric sequences.
Since phi is larger than phibar, unless the term of factor phi^n is null
\(and the sequence is thus a geometric sequence of ratio phibar),
this term will dominate the sum and the sequence will be asymptotically
equivalent to this term, a geometric sequence of ratio phi.
This is the case of the Fibonacci sequence starting with 0 and 1
\(or 1 and 1 if you count from next index).
|#

#|
Xophe sent me this formula. It is actually a variant of the formula in rfib,
using the Binomial Expansion Theorem twice and grouping terms.
It loses quite a bit in terms of performance, while regaining precision.
|#
(defun fact (n) (if (zerop n) 1 (* n (fact (1- n)))))
(defun fib-e (n)
  (declare (fixnum n))
  (/ (loop for k fixnum from 1 to n by 2
       sum (/ (* (expt 5 (/ (1- k) 2)) (fact n))
	      (fact k) (fact (- n k))))
     (expt 2 (1- n))))
#|
We thus see that refactoring in wrong ways
can be detrimental to your performance;
Or, considering the factoring in the opposite way,
we see how a seemingly unrelated computation
can be refactored into something we know how to do efficiently.

Of course, the problem of computing Fibonacci numbers
is rather simple, with only one parameter, so that
the direction in which to factor programs is straightforward.
In more complex problems, with many parameters,
programs may be factored in many ways,
and the choice of a more propitious factoring
may depend on the relative sizes and values of the actually given parameters.
Static benchmarking and dynamic monitoring can then help choose
an appropriate factoring depending on the parameters at hand.
|#

#|
Exercise: instead of expanding the exponent of a sum in rfib,
keep the factored version and give the exact answer
by working in the extension ring Z[phi].

Actually, if you start say with the the base (1,phi) of Z[phi]
and simplify based on conjugation between phi and 1-phi and other identities,
you may find that a heavily optimized version of this rfib
will yield the very same very-fast-fib as the heavily optimized version
of the matrix composition algorithm did. That's the Magic of Mathematics!

In any case, yet another different data representation can give
new directions to your coding.
You're never done refactoring your programs.
|#

#|
So, could you immediately recognize in very-fast-fib
the function originally defined by bogo-fib? No? What a shame!
Yet if and when you get a bit used to the refactoring techniques used above,
you will be able to match that pattern.
Now, Alan Perlis once said "You think you know when you can learn,
are more sure when you can write, even more when you can teach,
but certain when you can program."
http://www.cs.yale.edu/homes/perlis-alan/quotes.html
Therefore, for extra credit, so as to show
how deeply you understand these refactoring techniques, 
your next exercise will be to write
a metaprogram that helps automate such refactoring.
And afterwards, maybe you can write the fragments of a SSC
that puts these techniques to good use,
detecting when and how to invoke the corresponding metaprograms;
your SSC can use time to benchmark algorithms statically
and monitor programs dynamically so as to ensure
the most relevant algorithm is being used at any time.
Hence, when a user invokes bogo-fib into the enhanced system
with high optimization enabled, some variant of very-fast-fib will be invoked,
short-circuited by some well-tuned table lookup for small cases.
|#

#|
For more about Fibonacci numbers, you may ask Google,
that returned the following pages to me, among others:
http://www.cut-the-knot.org/arithmetic/Fibonacci.shtml
http://www.mcs.surrey.ac.uk/Personal/R.Knott/Fibonacci/fib.html
http://www.friesian.com/golden.htm
http://en.wikipedia.org/wiki/Fibonacci_number
http://ulcar.uml.edu/~iag/CS/Fibonacci.html
http://milan.milanovic.org/math/index.php
http://ccins.camosun.bc.ca/~jbritton/fibslide/jbfibslide.htm
http://plus.maths.org/issue3/fibonacci/
http://mathworld.wolfram.com/FibonacciNumber.html
http://dir.yahoo.com/Science/Mathematics/Numerical_Analysis/Numbers/Fibonacci/
|#

;;; Happy happy joy joy!!
