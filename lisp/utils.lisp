(defmacro $ (function &rest args)
  (let ((funcall-part
         (typecase function
           (symbol `(,function))
           (list (when (eq (first function) 'cl:function)
                   `(funcall ,function)))))
        (args-part
         (loop for rest on args
               for arg = (first rest)
               if (not (eq arg '$))
                 collect arg into result
               else
                 do (return (append result `(,rest)))
               end
               finally (return result))))
    (append funcall-part args-part)))

