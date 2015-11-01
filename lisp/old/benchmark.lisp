;;; with-gensyms
;;; usage: (with-gensyms (var) <body> ...)
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))

;;; Macro TIMES for benchmark
;;; usage: (times 5000 <body>)
;;; means: do 5000 times of <body> and calculate the total time.
(defmacro times (num &body body)
  (with-gensyms (i)
    `(time
      (dotimes (,i ,num)
        ,@body))))
