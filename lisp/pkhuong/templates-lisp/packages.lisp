(cl:defpackage #:template-lisp
  (:use #:common-lisp)
  (:export #:TLmacroexpand #:TLmacroexpand-1
	   #:add-macro #:TLcompile #:tl2cpp))