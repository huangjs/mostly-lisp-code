(cl:defpackage "S"
  (:use )
  (:export #:d ; dyn-bind
           #:c ; catch
           #:f ; closure (function)
           #:_ ; ignored value
   ))

(cl:defpackage "SERIALISABLE-CLOSURES"
  (:use #:cl #:sb-mop #:sb-walker)
  (:export #:serialisable-closure #:tag-of #:closed-vars-of #:build-expr-of
           #:slambda
           #:sfunction
           #:ensure-all-builders
           #:dyn-bind
           #:catch-frame))

(cl:defpackage "SERIALISABLE-CONTINUATIONS"
  (:use #:cl #:sb-cltl2 "SERIALISABLE-CLOSURES")
  (:export #:dynamic-binding #:var-of #:val-of
           #:catch-frame     #:tag-of
           #:capture
           #:bind #:dbind
           #:mprogn
           #:mlet* #:mdlet*
           #:mlet  #:mdlet
           #:mcatch
           #:mblock #:mreturn-from #:mreturn
           #:invoke-cont
           #:prune-redundant-frames))

(cl:defpackage "COMMON-COLD"
  (:use #:cl
        "SERIALISABLE-CLOSURES"
        "SERIALISABLE-CONTINUATIONS"
        #:hunchentoot
        #:cl-base64
        #:split-sequence)
  (:export #:slambda #:sfunction
           #:bind #:dbind
           #:mprogn
           #:mlet* #:mdlet*
           #:mlet  #:mdlet
           #:mcatch
           #:mblock #:mreturn-from #:mreturn
           #:ensure-all-builders
           #:register-key
           #:call-with-continuation-url
           #:send/suspend
           #:make-continuation-handler))
