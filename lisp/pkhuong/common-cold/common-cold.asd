(asdf:defsystem :common-cold
  :version "0.0.1"
  :depends-on (:hunchentoot ; ...
               :cl-base64 ; URL-encode continuations
               :zlib     ; compress continuations
               :ironclad ; encrypt continuatioms
               :split-sequence ; url parsing
               :sb-cltl2 ; compiler-let
               :cl-who   ; default error handler
               )
  :serial t
  :components ((:file "packages")
               (:file "closures") ; serialisable closures
               (:file "continuations") ; simple continuation monad
               (:file "handler")) ; wrap it all together for a simple interface
  )