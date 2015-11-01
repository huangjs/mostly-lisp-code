; (defmacro when (condition &rest body)
;   `(if ,condition (progn ,@body)))

; (defmacro unless (condition, &rest body)
;   `(if ,condition (progn ,@body)))

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

(print (loop for i from 1 to 10 collecting i))

(print (loop for i from 1 to 10 summing (expt i 2)))

(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou"))

;;; Fibonacci
(print (do ((n 0 (1+ n))
            (cur 0 next)
            (next 1 (+ cur next)))
           ((= 10 n) cur)))

(print
 (loop for i below 10 
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return a)))


;;; do-primes
(defun prime? (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (prime? n) return n))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
    ((> ,var ,end))
    ,@body))

;;; leak in the abstraction
;;; be aware that the end is evaluated many times.!
(macroexpand-1 `(do-primes (p 0 (random 100))
                 (format t "~d " p)))

;;; second time
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
    ((> ,var ending-value))
    ,@body))

;;; Unfortunately, this invokes two other leaks.
;;; 1. if start or end has side effects, evaluating them out of order can once again run afoul.
;;; 2. if you use a variable named "ending-value" like:
(macroexpand-1 `(do-primes (ending-value 0 10)
                 (print ending-value)))

;;; third time
;;; using unique symbol GENSYM
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
      ((> ,var ,ending-value-name))
      ,@body)))

;;; now expanding do-primes:
(macroexpand-1 `(do-primes (ending-value 0 10)
                 (print ending-value)))

;;; this is equivalent to
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
      ((> ,var ,ending-value-name))
      ,@body)))

(macroexpand-1 `(do-primes (ending-value 0 10)
                 (print ending-value)))

;;; using once-only
(defmacro do-primes ((var start end) &body body)
  (once-only (start end)
             `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
               ((> ,var ,end))
               ,@body)))

(macroexpand-1 `(do-primes (ending-value 0 10)
                 (print ending-value)))


;:*=======================
;:* with-gensyms
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))

(defmacro alias (new-function-name old-function-name)
    `(defun ,new-function-name (&rest args)
        (apply #',old-function-name args)))

(alias with-unique-names with-gensyms)
