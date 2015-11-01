#|
By Pierre R. Mai

Which is sound advice: Take for example an embedded language for the
implementation of an interpreter for a commercially relevant discrete
subset of the MATLAB/Simulink modelling language.  Simulink is mostly
based on individual blocks, which are connected together to form
models. The semantics can for the most part be reduced to the semantics
of the individual block types.

Hence an ideal language for implementing an interpreter for Simulink
models would allow one to define block types (here called node types,
for uninteresting reasons) directly, like e.g. so:
|#

(define-node-type discrete-filter ()
  (:match (10 "tllib/Discrete Filter"))
  (:parameters (numerator element-default-typed-vector "num")
               (denominator element-default-typed-vector "denom")
               (output-spec type-spec "output"))
  (:inputs (input typed-value))
  (:state
   (input-history (make-vector (1- (length numerator))
                               (convert 0.0d0 output-spec)))
   (output-history (make-vector (1- (length denominator))
                                (convert 0.0d0 output-spec))))
  (:outputs
   (output
    (let ((acc-spec (widen-type-spec input)))
      (typed-/
       (typed--
        (vector-accumulate numerator input-history acc-spec
						   :start1 1
						   :initial-value
						   (typed-* input (svref numerator 0) acc-spec))
        (vector-accumulate denominator output-history acc-spec :start1 1)
        acc-spec)
       (svref denominator 0)
       output-spec))))
  (:update-state
   (vector-shift-right input input-history)
   (vector-shift-right output output-history)))

#|
The actual implementation of the define-node-type macro ends up defining:

- a new CLOS class discrete-filter-node, which is an instance of a
special standard-node-class metaclass (with various MOP extensions)
with slots for the various parameter and state instance variables, as
well as "automatic" slots for the input and output vectors.
- Various methods specialized on this class for setup, output
calculation and state updating code, where the body of the methods is
wrapped in various flet, macrolet, with-slots, symbol-macrolet, and
handler-bind forms to provide a suitable environment for the execution
of the forms supplied by the :state, :outputs and :update-state forms.
- A couple of methods eql-specialized the class itself for parameter
parsing and instance allocation purposes, as well as for matching of
this node to blocks in the model, or for dependency analysis and
circularity detection logic.

So the actual implementation of the functionality provided by
define-node-type is mostly achieved through functional libraries, MOP
hackery, etc., and it is indeed still possible to define new node types
 by hand via defclass, defmethod and various globally-defined macro
helpers, but a node definition is much more concise (usually < 1 page
of code vs. >=3 pages of code).

For the most part, this post is also intended to give another example
for what macros are used for in practice, for those who are really
interested, and not just wanting to convince themselves that macros (or
Lisp in general) has nothing interesting to offer them, which is fine
by me, but would of course be less distracting if they did this in some
group other than c.l.l.

Personally, what I get from Lisp's macros is the ability to easily
define new languages to my liking and suitable for the problem at hand,
without most of the tedious parts of the compiler construction business
this would imply in other languages, and program in them, which results
in compact, to-the-point code, rather than trying to use a
general-purpose language to do the same by foot, which results in lots
of boiler-plate code, with the interesting bits sprinkled in between,
in usually homeopathic doses.

This freedom also results in a different approach to design and
programming, IMHO: When faced with a problem like the one above, I
usually sit down, and analyze the problem space until I can start to
envisage the language I want to solve it in. That is, I would start to
write the actual define-node-type form above, and massage it until it
seemed to capture the intent I want to convey, for a couple of
interesting cases. Only then would I start to think about the macrology
and the infrastructure needed to implement define-node-type, and start
design and implementation of the domain-specific language.

Regs, Pierre.

PS: For other instances of practical uses to which macros are put, one
can look at many interesting libraries on common-lisp.net, or for
example at most of the internals of the SBCL or CMUCL compilers, which
would be much more verbose, without the use of macros (though some uses
of macros in Python-the-compiler could nowadays be replaced with simple
CLOS code, many cannot).  
|#
