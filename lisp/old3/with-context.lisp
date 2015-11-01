;; Passing lexical context to macros with MACROLET
;; 
;; Michael Weber <michaelw@foldr.org>
;; 2007-05-22
(defmacro with-context (context &body body)
  `(macrolet ((get-context ()
                ,context))
     ,@body))

(defmacro inner (arg &environment env)
  (multiple-value-bind (context expandedp)
      (macroexpand '(get-context) env)
    `'(invoke-inner ,(if expandedp
                         (getf context arg)
                         arg))))
#||
(with-context '(:foo zero :bar one)
  (values (inner :foo)
          (inner :bar)))
=> (INVOKE-INNER ZERO), (INVOKE-INNER ONE)

(with-context '(:foo zero :bar one)
  (with-context '(:foo 0 :bar 1)
    (values (inner :foo)
            (inner :bar))))
=> (INVOKE-INNER 0), (INVOKE-INNER 1)

(inner :foo)
=> (INVOKE-INNER :FOO)
||#
