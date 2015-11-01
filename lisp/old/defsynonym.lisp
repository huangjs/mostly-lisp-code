;;; macros that defines macros

(defmacro defsynonym (new-name old-name)
  "Define OLD-NAME to be equivalent to NEW-NAME when used in
the first position of a LISP form."
  `(defmacro ,new-name (&rest args)
    `(,',old-name ,@args)))
