(defun hello-world ()
  (princ "Hello world!")
  (terpri))

(hello-world)

(use-package "FFI")
     
(Clines
 "   int tak(x, y, z)                       "
 "   int x, y, z;                           "
 "   {   if (y >= x) return(z);             "
 "       else return(tak(tak(x-1, y, z),    "
 "                       tak(y-1, z, x),    "
 "                       tak(z-1, x, y)));  "
 "   }                                      "
 )
     
(defun tak (x y z)
  (c-inline (x y z) (:int :int :int) :int
			"tak(#0,#1,#2)" :one-liner t))

