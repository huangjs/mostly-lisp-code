;;; template for writing with-foo macros


(defmacro with-foo ((foo) &body body)
  `(call-with-foo (lambda (,foo) ,@body)))

(defun call-with-foo (function)
  (let (foo)
    (unwind-protect
        (funcall function (setf foo (get-foo)))
      (when foo (release-foo foo)))))


#|

***** `WITH-' and `CALL-WITH-': Dynamic State and Cleanup

Prefix `WITH-' to any procedure that establishes dynamic state and
calls a nullary procedure, which should be the last (required)
argument.  The dynamic state should be established for the extent of
the nullary procedure, and should be returned to its original state
after that procedure returns.

  Examples: with-input-from-file with-output-to-file

  Exception:  Some systems provide a procedure (WITH-CONTINUATION
  <continuation> <thunk>), which calls <thunk> in the given
  continuation, using that continuation's dynamic state.  If <thunk>
  returns, it will return to <continuation>, not to the continuation of
  the call to WITH-CONTINUATION.  This is acceptable.

Prefix `CALL-WITH-' to any procedure that calls a procedure, which
should be its last argument, with some arguments, and is either somehow
dependent upon the dynamic state or continuation of the program, or
will perform some action to clean up data after the procedure argument
returns.  Generally, `CALL-WITH-' procedures should return the values
that the procedure argument returns, after performing the cleaning
action.

  Examples:

  - CALL-WITH-INPUT-FILE and CALL-WITH-OUTPUT-FILE both accept a
    pathname and a procedure as an argument, open that pathname (for
    input or output, respectively), and call the procedure with one
    argument, a port corresponding with the file named by the given
    pathname.  After the procedure returns, CALL-WITH-INPUT-FILE and
    CALL-WITH-OUTPUT-FILE close the file that they opened, and return
    whatever the procedure returned.

  - CALL-WITH-CURRENT-CONTINUATION is dependent on the continuation
    with which it was called, and passes as an argument an escape
    procedure corresponding with that continuation.

  - CALL-WITH-OUTPUT-STRING, a common but non-standard procedure
    definable in terms of OPEN-OUTPUT-STRING and GET-OUTPUT-STRING from
    SRFI 6 (Basic String Ports), calls its procedure argument with an
    output port, and returns a string of all of the output written to
    that port.  Note that it does not return what the procedure
    argument returns, which is an exception to the above rule.

Generally, the distinction between these two classes of procedures is
that `CALL-WITH-...' procedures should not establish fresh dynamic
state and instead pass explicit arguments to their procedure arguments,
whereas `WITH-...' should do the opposite and establish dynamic state
while passing zero arguments to their procedure arguments.

|#
